#![allow(dead_code)]

use egui::{FontId, Response, RichText, TextEdit};

pub struct LayoutBuilder {
    layout: egui::text::LayoutJob
}

impl LayoutBuilder {
    pub fn text(mut self, text: &str) -> Self {
        self.layout.append(text, 0.0, egui::TextFormat::default());
        self
    }

    pub fn sized(mut self, text: &str, size: f32) -> Self {
        self.layout.append(text, 0.0, egui::TextFormat {
            font_id: egui::FontId::proportional(size),
            ..Default::default()
        });
        self
    }

    pub fn mono(mut self, text: &str, size: f32) -> Self {
        self.layout.append(text, 0.0, egui::TextFormat {
            font_id: egui::FontId::monospace(size),
            ..Default::default()
        });
        self
    }
}

pub trait UiExtensions {
    fn sized(&mut self, text: impl Into<String>, size: f32) -> Response;
    fn mixed(&mut self, f: impl FnOnce(LayoutBuilder) -> LayoutBuilder) -> Response;
    fn clip_label(&mut self, text: impl Into<String>, font: FontId) -> Response;
}

impl UiExtensions for egui::Ui {
    fn sized(&mut self, text: impl Into<String>, size: f32) -> Response {
        self.add(egui::Label::new(RichText::new(text).size(size)))
    }

    fn mixed(&mut self, f: impl FnOnce(LayoutBuilder) -> LayoutBuilder) -> Response {
        let builder = LayoutBuilder { layout: egui::text::LayoutJob::default() };
        let builder = f(builder);
        self.add(egui::Label::new(builder.layout))
    }

    fn clip_label(&mut self, text: impl Into<String>, font: FontId) -> Response {
        self.add(TextEdit::singleline(&mut text.into())
            .margin((0., 0.).into())
            .interactive(false)
            .frame(false)
            .clip_text(true)
            .font(font)
            .text_color(self.style().noninteractive().fg_stroke.color)
        )
    }
}

struct FilterBoxOption {
    value: String,
    display_name: String,
    filterable_name: String,
}


pub struct FilterBoxOptions {
    options: Vec<FilterBoxOption>
}

impl FilterBoxOptions {
    pub fn add(&mut self, value: String, display_name: String) {
        self.options.push(FilterBoxOption {
            filterable_name: display_name.clone(),
            value,
            display_name,
        });
    }
}


pub struct FilterBoxState {
    id: String,
    selected: Option<String>,
    filter: String
}

impl FilterBoxState {
    pub fn new(id: String, selected: Option<String>) -> FilterBoxState {
        FilterBoxState {
            id,
            selected,
            filter: String::from("")
        }
    }

    pub fn show<T, F: FnOnce(&mut FilterBoxOptions) -> T>(&mut self, ui: &mut egui::Ui, f: F) -> egui::InnerResponse<Option<T>> {
        egui::ComboBox::from_id_source(&self.id)
            .selected_text(self.selected.as_ref().unwrap_or_else(|| &self.filter).clone())
            .wrap(true)
            .width(ui.available_width())
            .show_ui(ui, |ui| {
                let mut options = FilterBoxOptions { options: vec![] };
                let ret = f(&mut options);

                for option in options.options {
                    if option.filterable_name.to_lowercase().contains(&self.filter) {
                        let is_selected = self.selected.as_ref().is_some_and(|name| name == &option.value);
                        if ui.selectable_label(is_selected, option.display_name).clicked() {
                            self.selected = Some(option.value);
                            self.filter = String::new();
                        }
                    }
                }

                ui.input(|state| {
                    for event in state.events.iter() {
                        match event {
                            egui::Event::Text(text) => {
                                self.selected = None;
                                self.filter.push_str(text);
                            }
                            egui::Event::Key {
                                key: egui::Key::Backspace,
                                pressed: true,
                                ..
                            } => {
                                self.filter.pop();
                            }
                            _ => { }
                        }
                    }
                });

                ret
            })
    }

    pub fn get_selected(&self) -> Option<&String> {
        self.selected.as_ref()
    }
}