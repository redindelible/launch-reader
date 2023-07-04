mod header_parser;

mod cache;
mod egui_extensions;
mod themes;
mod task;
mod table;

use std::fs::File;
use std::io::Read;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::Duration;

use eframe::{App, CreationContext, emath::Align};
use egui::{Context, PointerButton, RichText, containers::Frame, Margin, TextStyle, FontId, Layout, ScrollArea};

use native_dialog::FileDialog;
use serde_derive::{Serialize, Deserialize};
use lz4_flex;

use crate::egui_extensions::{FilterBoxState, UiExtensions};
use crate::cache::{Cache, CacheHandle, AccessRequest};
use crate::header_parser::parse_and_load;
use crate::table::Table;
use crate::task::{ProgressTask, Task};
use crate::themes::{get_theme, Themes};

type RecentFiles = Vec<(PathBuf, String)>;

#[derive(Serialize, Deserialize, Clone)]
struct CachedPacket {
    hash: String,
    compressed: Vec<u8>
}

impl CachedPacket {
    fn new(hash: String, text: String) -> CachedPacket {
        let compressed = lz4_flex::compress_prepend_size(text.as_bytes());
        CachedPacket { hash, compressed }
    }

    fn decompress(&self) -> Option<String> {
        String::from_utf8(lz4_flex::decompress_size_prepended(&self.compressed).ok()?).ok()
    }
}

#[derive(Serialize, Deserialize, Default)]
struct CacheFormat {
    recent_files: RecentFiles,
    recent_packet: Vec<CachedPacket>,
}


#[derive(Clone, Eq, PartialEq, Hash)]
enum SourceState {
    NoFileSelected,
    SelectedFile((PathBuf, String))
}

impl SourceState {
    fn is_selected(&self) -> bool {
        match self {
            SourceState::NoFileSelected => false,
            SourceState::SelectedFile(_) => true
        }
    }

    fn get_file(&self) -> &Path {
        match self {
            SourceState::NoFileSelected => panic!(),
            SourceState::SelectedFile((path, _)) => path
        }
    }
}

#[derive(Clone)]
enum PacketState {
    NoPacketSelected,
    SelectedPacket(CachedPacket)
}

impl PacketState {
    fn is_selected(&self) -> bool {
        match self {
            PacketState::NoPacketSelected => false,
            PacketState::SelectedPacket(_) => true
        }
    }

    fn get_packet(&self) -> &CachedPacket {
        match self {
            PacketState::NoPacketSelected => panic!(),
            PacketState::SelectedPacket(packet) => packet
        }
    }
}

enum HeaderState {
    None,
    Acquiring(PathBuf, Task<Option<String>>),
    Acquired(PathBuf, String),
    Errored(PathBuf),
}

impl HeaderState {
    fn is_for_file(&self, f: &Path) -> bool {
        match self {
            HeaderState::None => false,
            HeaderState::Acquiring(p, _)
            | HeaderState::Acquired(p, _)
            | HeaderState::Errored(p)
                => p == f,
        }
    }

    fn progress(&mut self) {
        if let HeaderState::Acquiring(_, task) = self {
            if task.is_ready() {
                let HeaderState::Acquiring(path, task) = std::mem::replace(self, HeaderState::None) else { unreachable!() };
                let new_state = match task.result() {
                    Some(hash) => HeaderState::Acquired(path, hash),
                    None => HeaderState::Errored(path)
                };
                *self = new_state;
            }
        }
    }
}

enum PreviewState {
    None,
    Error { hash: String },
    File { hash: String, contents: String },
}

impl PreviewState {
    fn has_hash(&self, check: &str) -> bool {
        match self {
            PreviewState::None => false,
            PreviewState::Error { hash } | PreviewState::File { hash, .. } => hash == check
        }
    }
}

struct SourceTab {
    source_state: SourceState,
    recent_files: Vec<(PathBuf, String)>,

    packet_state: PacketState,
    use_header: bool,
    header_state: HeaderState,
    recent_packet: Vec<CachedPacket>,
    loading_packet: Option<Task<Option<CachedPacket>>>,

    preview: PreviewState,

    cache: CacheHandle<CacheFormat>,
    recent_files_request: Option<AccessRequest<RecentFiles>>,
    recent_packet_request: Option<AccessRequest<Vec<CachedPacket>>>,
}

impl SourceTab {
    fn new(cache: &CacheHandle<CacheFormat>) -> Self {
        let recent_files_request = cache.read(|cache| cache.recent_files.clone());
        let recent_packet_request = cache.read(|cache| cache.recent_packet.clone());

        SourceTab {
            source_state: SourceState::NoFileSelected,
            recent_files: vec![],

            packet_state: PacketState::NoPacketSelected,
            use_header: false,
            header_state: HeaderState::None,
            recent_packet: vec![],
            loading_packet: None,

            preview: PreviewState::None,

            cache: cache.clone(),
            recent_files_request: Some(recent_files_request),
            recent_packet_request: Some(recent_packet_request),
        }
    }

    fn select_file(&mut self, pair: (PathBuf, String)) {
        let item = self.recent_files.iter().enumerate().find_map(|item| if &item.1.1 == &pair.1 { Some(item.0)} else { None });

        if let Some(idx) = item {
            let pair = self.recent_files.remove(idx);
            self.recent_files.insert(0, pair);
        } else {
            self.recent_files.insert(0, pair.clone());
        }

        if self.recent_files.len() > 5 {
            self.recent_files.drain(5..);
        }

        let recent = self.recent_files.clone();

        self.cache.update(move |cache| {
            cache.recent_files = recent;
        });

        self.source_state = SourceState::SelectedFile(pair);
    }

    fn select_packet(&mut self, packet: CachedPacket) {
        let idx = self.recent_packet.iter().position(|p| p.hash == packet.hash);

        if let Some(idx) = idx {
            let packet = self.recent_packet.remove(idx);
            self.recent_packet.insert(0, packet);
        } else {
            self.recent_packet.insert(0, packet.clone());
        }

        while self.recent_packet.len() > 5 {
            self.recent_packet.pop();
        }

        let recent = self.recent_packet.clone();

        self.cache.update(|cache| {
            cache.recent_packet = recent;
        });

        self.packet_state = PacketState::SelectedPacket(packet);
    }

    fn update(&mut self, ctx: &Context, ui: &mut egui::Ui, app_state: &mut AppState) {
        if let Some(req) = &self.recent_files_request {
            if req.is_ready() {
                self.recent_files = req.fetch().unwrap().clone();
                self.recent_files_request = None;
            }
        }

        if let Some(req) = &self.recent_packet_request {
            if req.is_ready() {
                self.recent_packet = req.fetch().unwrap().clone();
                self.recent_packet_request = None;
            }
        }

        if let Some(task) = &self.loading_packet {
            if task.is_ready() {
                let task = self.loading_packet.take().unwrap().result();
                if let Some(cached_packet) = task {
                    self.select_packet(cached_packet);
                }
            }
        }

        ui.vertical(|ui| {
            if let SourceState::SelectedFile((file, name)) = &self.source_state {
                let text = String::from("Selected Source: ") + &file.file_name().unwrap().to_string_lossy();
                ui.clip_label(text, FontId::proportional(13.))
                    .on_hover_text(name);
            } else {
                ui.sized("Selected Source: None", 13.);
            }

            ui.add_space(5.);

            ui.with_layout(Layout::top_down_justified(Align::Min), |ui| {
                // change the background of buttons
                ui.style_mut().visuals.widgets.inactive.weak_bg_fill = ui.style().visuals.panel_fill;

                if !self.recent_files.is_empty() {
                    ui.sized("Recent Files:", 14.);
                    ScrollArea::vertical().max_height(130.).show(ui, |ui| {
                        for pair in &self.recent_files.clone() {
                            let button = egui::Button::new(RichText::new(&pair.1).monospace());
                            if ui.add(button).clicked() {
                                self.select_file(pair.clone());
                            }
                        }
                    });

                    ui.add_space(4.);
                }

                if ui.add(egui::Button::new("+ Add a File")).clicked() {
                    let file = FileDialog::new()
                        .set_location("~")
                        .show_open_single_file();
                    match file.ok().flatten() {
                        Some(file) => {
                            let name = file.display().to_string();
                            self.select_file((file, name));
                        },
                        None => {}
                    }
                }
            });

            ui.separator();

            ui.with_layout(Layout::top_down_justified(Align::Min), |ui| {
                let bar = (ui.available_width(), 20.).into();
                ui.allocate_ui_with_layout(bar, Layout::left_to_right(Align::Center), |ui| {
                    ui.checkbox(&mut self.use_header, "Use Header?");

                    if self.use_header && self.source_state.is_selected() {
                        let file = self.source_state.get_file().to_path_buf();
                        if !self.header_state.is_for_file(&file) {
                            let file_clone = file.clone();
                            let task = Task::new(move || {
                                let mut file = File::open(file_clone).ok()?;
                                let mut header = [0; 41];
                                file.read(&mut header).ok().and_then(|n| (n == 41).then_some(()))?;
                                Some(String::from_utf8(header.to_vec()).ok()?)
                            });
                            self.header_state = HeaderState::Acquiring(file, task)
                        }

                        self.header_state.progress();

                        match &self.header_state {
                            HeaderState::None => unreachable!(),
                            HeaderState::Acquiring(_, _) => {
                                ui.spinner();
                            }
                            HeaderState::Acquired(_, hash) => {
                                ui.clip_label(format!("{}...", &hash[..8]), FontId::monospace(12.)).on_hover_text(hash);
                            }
                            HeaderState::Errored(_) => {
                                ui.sized("Error in Header.", 12.);
                            }
                        }
                    }
                });

                if let PacketState::SelectedPacket(cached) = &self.packet_state {
                    let text = format!("Selected Packet: {}...", &cached.hash[..8]);
                    ui.clip_label(text, FontId::proportional(13.))
                        .on_hover_text(&cached.hash);
                } else {
                    ui.sized("Selected Packet: None", 13.);
                }

                ui.scope(|ui| {
                    // change the background of buttons
                    ui.style_mut().visuals.widgets.inactive.weak_bg_fill = ui.style().visuals.panel_fill;

                    if !self.recent_packet.is_empty() {
                        ui.sized("Recent Packets:", 14.);
                        ScrollArea::vertical().max_height(130.).show(ui, |ui| {
                            for cached_packet in &self.recent_packet.clone() {
                                let text = format!("{}...", &cached_packet.hash[..8]);
                                let button = egui::Button::new(RichText::new(text).monospace());
                                if ui.add(button).on_hover_text(&cached_packet.hash).clicked() {
                                    self.select_packet(cached_packet.clone());
                                }
                            }
                        });

                        ui.add_space(4.);
                    }
                });

                ui.columns(3, |columns| {
                    columns[0].vertical_centered_justified(|ui| {
                        if ui.button("From File").clicked() {
                            let file = FileDialog::new()
                                .set_location("~")
                                .show_open_single_file();
                            match file.ok().flatten() {
                                Some(file) => {
                                    self.loading_packet = Some(Task::new(move || {
                                        let output = Command::new("git").arg("hash-object").arg(&file).output();
                                        let hash = String::from_utf8(output.ok()?.stdout).ok()?.trim().to_string();
                                        let text = std::fs::read_to_string(file).ok()?;
                                        Some(CachedPacket::new(hash, text))
                                    }));
                                },
                                None => {}
                            }
                        }
                    });

                    columns[1].vertical_centered_justified(|ui| {
                        if ui.button("From URL").clicked() {

                        }
                    });

                    columns[2].vertical_centered_justified(|ui| {
                        if ui.add_enabled(self.packet_state.is_selected(), egui::Button::new("Preview")).clicked() {
                            let packet = self.packet_state.get_packet();
                            if self.preview.has_hash(&packet.hash) {
                                self.preview = PreviewState::None;
                            } else {
                                if let Some(text) = packet.decompress() {
                                    self.preview = PreviewState::File { hash: packet.hash.clone(), contents: text };
                                } else {
                                    self.preview = PreviewState::Error { hash: packet.hash.clone() };
                                }
                            }
                        }
                    });
                });
            });

            ui.separator();

            ui.with_layout(Layout::bottom_up(Align::Min), |ui| {
                let bar = (ui.available_width(), 30.).into();
                ui.allocate_ui_with_layout(bar, Layout::right_to_left(Align::Center), |ui| {
                    ui.spacing_mut().button_padding = (15., 5.).into();

                    let should_enable = self.source_state.is_selected() && self.packet_state.is_selected();

                    let button = egui::Button::new(RichText::new("Load").size(15.));
                    if ui.add_enabled(should_enable, button)
                        .on_disabled_hover_text("Select a Source and a Packet Format")
                        .clicked() {

                        let source = self.source_state.get_file().to_path_buf();
                        let packet = self.packet_state.get_packet().decompress();

                        app_state.source_loading = SourceLoadingState::Loading(ProgressTask::new(move |tracker| {
                            let packet = packet.ok_or_else(|| String::from("Could not load packet format."))?;
                            let mut file = File::open(source).map_err(|_| String::from("Could not open source file."))?;

                            let mut buffer = Vec::new();
                            file.read_to_end(&mut buffer).map_err(|_| String::from("Could not read from source file."))?;

                            parse_and_load(&packet, &buffer, &tracker)
                        }));
                    }

                    match &app_state.source_loading {
                        SourceLoadingState::None => { }
                        SourceLoadingState::Loading(task) => {
                            ui.add(egui::ProgressBar::new(task.progress()).show_percentage());
                            ctx.request_repaint_after(Duration::from_millis(1000));
                        }
                        SourceLoadingState::Errored(text) => {
                            ui.with_layout(Layout::left_to_right(Align::Center), |ui| {
                                ui.clip_label(format!("Error: {}", text), FontId::proportional(13.)).on_hover_text(text);
                            });
                        }
                    }
                });
            });
        });

        let mut should_be_open = true;
        match &self.preview {
            PreviewState::File { hash, contents } => {
                egui::Window::new(hash)
                    .open(&mut should_be_open)
                    .show(ctx, |ui| {
                        ui.with_layout(Layout::top_down_justified(Align::Min), |ui| {
                            ScrollArea::both().show(ui, |ui| {
                                ui.add(egui::Label::new(RichText::new(contents).monospace()).wrap(false));
                            });
                        });
                });
            }
            PreviewState::Error { hash } => {
                egui::Window::new(hash)
                    .resizable(false)
                    .collapsible(false)
                    .open(&mut should_be_open)
                    .show(ctx, |ui| {
                        ui.centered_and_justified(|ui| {
                            ui.label("Error when opening this packet format.")
                        });
                });
            }
            PreviewState::None => { }
        }
        if !should_be_open {
            self.preview = PreviewState::None;
        }
    }
}

struct SettingsTab {
    cache: CacheHandle<CacheFormat>
}

impl SettingsTab {
    fn new(cache: &CacheHandle<CacheFormat>) -> Self {
        SettingsTab {
            cache: cache.clone()
        }
    }

    fn update(&mut self, _ctx: &Context, ui: &mut egui::Ui) {
        if ui.button("Clear Cache").clicked() {
            self.cache.clear();
        }
    }
}

#[derive(Copy, Clone)]
enum PlotEditorAction {
    Add,
    Edit(usize)
}

struct PlotEditor {
    is_open: bool,
    name: String,

    x_axis: FilterBoxState,

    y_axis: FilterBoxState,

    sample: u32,

    action: PlotEditorAction
}

impl PlotEditor {
    fn new_closed() -> PlotEditor {
        PlotEditor {
            is_open: false,
            name: String::new(),
            x_axis: FilterBoxState::new("x axis filter".to_string(), None),
            y_axis: FilterBoxState::new("y axis filter".to_string(), None),
            sample: 1,
            action: PlotEditorAction::Add,
        }
    }

    fn new_add() -> PlotEditor {
        PlotEditor {
            is_open: true,
            name: String::new(),
            x_axis: FilterBoxState::new("x axis filter".to_string(), None),
            y_axis: FilterBoxState::new("y axis filter".to_string(), None),
            sample: 1,
            action: PlotEditorAction::Add,
        }
    }

    fn new_edit(idx: usize, old: &PlotInfo) -> PlotEditor {
        PlotEditor {
            is_open: true,
            name: old.name.clone(),
            x_axis: FilterBoxState::new("x axis filter".to_string(), Some(old.x_column.clone())),
            y_axis: FilterBoxState::new("y axis filter".to_string(), Some(old.y_column.clone())),
            sample: old.sample,
            action: PlotEditorAction::Edit(idx)
        }
    }

    fn create(&self, table: &Table) -> (PlotInfo, Vec<[f64; 2]>) {
        let x_column = self.x_axis.get_selected().unwrap().clone();
        let y_column = self.y_axis.get_selected().unwrap().clone();

        let x_points = table.column(&x_column).as_f64();
        let y_points = table.column(&y_column).as_f64();

        let plot_points = x_points.zip(y_points)
            .step_by(self.sample as usize)
            .map(|(x, y)| [x, y]).collect();

        let name = if self.name.is_empty() { format!("{} / {}", &x_column[..4], &y_column[..4]) } else { self.name.clone() };

        (PlotInfo { name, x_column, y_column, sample: self.sample }, plot_points)
    }
}

struct PlotInfo {
    name: String,
    x_column: String,
    y_column: String,
    sample: u32,
}

struct PlotTab {
    plots: Vec<PlotInfo>,
    selected: Option<usize>,

    editor: PlotEditor,
}

impl PlotTab {
    fn new() -> Self {
        PlotTab {
            plots: vec![],
            selected: None,
            editor: PlotEditor::new_closed()
        }
    }

    fn update(&mut self, _ctx: &Context, ui: &mut egui::Ui, app_state: &mut AppState) {
        match &mut app_state.plot {
            PlotState::Inactive => {
                ui.centered_and_justified(|ui| {
                    ui.sized("No data to plot.", 15.);
                });
            }
            PlotState::Active { table, lines } => {
                ui.with_layout(Layout::top_down_justified(Align::Min), |ui| {
                    ui.sized("Plots:", 15.);
                    ScrollArea::vertical().max_height(130.).show(ui, |ui| {
                        for (i, plot) in self.plots.iter().enumerate() {
                            if ui.selectable_label(
                                self.selected.is_some_and(|v| v == i),
                                &plot.name).clicked() {
                                self.selected = Some(i);
                            }
                        }
                    });
                    if let Some(i) = self.selected {
                        ui.sized(format!("Selected: {}", &self.plots[i].name), 15.);
                    } else {
                        ui.sized(format!("Selected: None"), 15.);
                    }
                    ui.columns(3, |columns| {
                        columns[0].vertical_centered_justified(|ui| {
                            if ui.add(egui::Button::new("Add")).clicked() {
                                self.editor = PlotEditor::new_add();
                            }
                        });
                        columns[1].vertical_centered_justified(|ui| {
                            if ui.add_enabled(self.selected.is_some(), egui::Button::new("Edit")).clicked() {
                                let idx = self.selected.unwrap();
                                self.editor = PlotEditor::new_edit(idx, &self.plots[idx]);
                            }
                        });
                        columns[2].vertical_centered_justified(|ui| {
                            if ui.add_enabled(self.selected.is_some(), egui::Button::new("Remove")).clicked() {
                                let idx = self.selected.unwrap();
                                lines.remove(idx);
                                self.plots.remove(idx);
                                self.selected = None;

                                if matches!(self.editor.action, PlotEditorAction::Edit(i) if i == idx) {
                                    self.editor = PlotEditor::new_closed();
                                }
                            }
                        });
                    });

                    ui.separator();

                    if self.editor.is_open {
                        ui.with_layout(Layout::left_to_right(Align::Min), |ui| {
                            ui.label("Name:");
                            ui.add(egui::TextEdit::singleline(&mut self.editor.name));
                        });

                        ui.add_space(4.);

                        ui.group(|ui| {
                            ui.horizontal_top(|ui| {
                                ui.label("X-axis:");
                                self.editor.x_axis.show(ui, |options| {
                                    for (column_name, column_type) in table.schema().column_types() {
                                        let display = format!("{} ({})", column_name, column_type);
                                        options.add(column_name.to_string(), display);
                                    }
                                });
                            });
                        });

                        ui.add_space(4.);

                        ui.group(|ui| {
                            ui.horizontal_top(|ui| {
                                ui.label("Y-axis:");
                                self.editor.y_axis.show(ui, |options| {
                                    for (column_name, column_type) in table.schema().column_types() {
                                        let display = format!("{} ({})", column_name, column_type);
                                        options.add(column_name.to_string(), display);
                                    }
                                });
                            });
                        });

                        ui.add_space(4.);

                        ui.with_layout(Layout::left_to_right(Align::Min).with_main_justify(false), |ui| {
                            ui.label("Sampling:");
                            ui.add(egui::DragValue::new(&mut self.editor.sample).clamp_range(1..=1_000_000));
                        });

                        ui.with_layout(Layout::bottom_up(Align::Max), |ui| {
                            match self.editor.action {
                                PlotEditorAction::Add => {
                                    let is_enabled = self.editor.x_axis.get_selected().is_some() && self.editor.y_axis.get_selected().is_some();
                                    if ui.add_enabled(is_enabled, egui::Button::new("Add")).clicked() {
                                        let (info, points) = self.editor.create(&table);
                                        lines.push(points);
                                        self.plots.push(info);
                                    }
                                }
                                PlotEditorAction::Edit(idx) => {
                                    let is_enabled = self.editor.x_axis.get_selected().is_some() && self.editor.y_axis.get_selected().is_some();
                                    if ui.add_enabled(is_enabled, egui::Button::new("Update")).clicked() {
                                        let (info, points) = self.editor.create(&table);
                                        lines[idx] = points;
                                        self.plots[idx] = info;
                                    }
                                }
                            }
                        });
                    }
                });
            }
        }
    }
}

#[derive(Eq, PartialEq, Copy, Clone, Hash)]
enum ActiveTab {
    Source,
    Data,
    Settings
}

enum SourceLoadingState {
    None,
    Loading(ProgressTask<Result<Table, String>>),
    Errored(String)
}

enum PlotState {
    Inactive,
    Active {
        table: Table,
        lines: Vec<Vec<[f64; 2]>>
    }
}

struct AppState {
    plot: PlotState,

    source_loading: SourceLoadingState
}


struct FV2App {
    source_tab: SourceTab,
    settings_tab: SettingsTab,
    plot_tab: PlotTab,
    active: ActiveTab,

    state: AppState,
}

impl FV2App {
    fn new(cache_handle: CacheHandle<CacheFormat>, _cc: &CreationContext) -> Self {
        FV2App {
            source_tab: SourceTab::new(&cache_handle),
            settings_tab: SettingsTab::new(&cache_handle),
            plot_tab: PlotTab::new(),
            active: ActiveTab::Source,
            state: AppState {
                plot: PlotState::Inactive,
                source_loading: SourceLoadingState::None
            }
        }
    }
}

impl App for FV2App {
    fn update(&mut self, ctx: &Context, _frame: &mut eframe::Frame) {
        if let SourceLoadingState::Loading(task) = &self.state.source_loading {
            if task.is_ready() {
                let SourceLoadingState::Loading(task) = std::mem::replace(&mut self.state.source_loading, SourceLoadingState::None) else { panic!() };
                match task.result() {
                    Ok(table) => {
                        // let xs = table.column("highG_data.timeStamp_highG");
                        // let ys = table.column("highG_data.hg_az");
                        //
                        // let line: Vec<[f64; 2]> = xs.as_u32().iter().zip(ys.as_f32()).map(|(time, alt)| [*time as f64, *alt as f64]).collect();

                        self.state.plot = PlotState::Active {
                            table,
                            lines: vec![]
                            // lines: vec![line]
                        };

                        self.active = ActiveTab::Data
                    },
                    Err(message) => {
                        self.state.source_loading = SourceLoadingState::Errored(message);
                    }
                }
            }
        }

        ctx.set_style(get_theme(Themes::Dark));

        let frame = Frame::side_top_panel(&ctx.style()).inner_margin(Margin { left: 1., right: 1., top: 0., bottom: 0.});
        egui::SidePanel::right("settings_panel").exact_width(240.).resizable(false).frame(frame).show(ctx, |ui| {
            ui.scope(|ui| {
                ui.spacing_mut().item_spacing = (0., 0.).into();
                ui.spacing_mut().button_padding = (0., 5.).into();

                ui.style_mut().visuals.widgets.hovered.bg_stroke.width = 0.;
                ui.style_mut().visuals.widgets.active.bg_stroke.width = 0.;
                ui.style_mut().visuals.widgets.hovered.expansion = 0.;

                ui.style_mut().text_styles.get_mut(&TextStyle::Button).unwrap().size = 15.;

                ui.columns(3, |columns| {
                    columns[0].vertical_centered_justified(|ui| ui.selectable_value(&mut self.active, ActiveTab::Source, "Source"));
                    columns[1].vertical_centered_justified(|ui| ui.selectable_value(&mut self.active, ActiveTab::Data, "Plot"));
                    columns[2].vertical_centered_justified(|ui| ui.selectable_value(&mut self.active, ActiveTab::Settings, "Settings"));
                });

                ui.add(egui::Separator::default().spacing(0.));
            });

            egui::CentralPanel::default().show_inside(ui, |ui| {
                match self.active {
                    ActiveTab::Source => self.source_tab.update(ctx, ui, &mut self.state),
                    ActiveTab::Settings => self.settings_tab.update(ctx, ui),
                    ActiveTab::Data => self.plot_tab.update(ctx, ui, &mut self.state),
                };
            });
        });

        let frame = Frame::central_panel(&ctx.style()).inner_margin(0.);

        egui::CentralPanel::default().frame(frame).show(ctx, |ui| {
            match &self.state.plot {
                PlotState::Inactive => {
                    ui.centered_and_justified(|ui| {
                        ui.sized("Select a Source", 40.);
                    });
                }
                PlotState::Active { lines, .. } => {
                    ui.spacing_mut().item_spacing = (20., 20.).into();

                    egui::widgets::plot::Plot::new("data_plot")
                        .allow_drag(false)
                        .allow_scroll(false)
                        .boxed_zoom_pointer_button(PointerButton::Primary)
                        .set_margin_fraction((0.1, 0.1).into())
                        .show(ui, |plot| {
                            for line in lines {
                                plot.points(egui::plot::Points::new(line.clone()))
                            }
                        });
                }
            }
        });
    }
}


fn main() {
    let cache = Cache::start_caching().unwrap_or_else(|| CacheHandle::new_disconnected());

    let mut native_options = eframe::NativeOptions::default();
    native_options.initial_window_size = Some((1000., 500.).into());
    eframe::run_native("FV2", native_options, Box::new(|cc| Box::new(FV2App::new(cache, cc)))).unwrap()
}
