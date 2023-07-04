#![allow(dead_code)]

use std::sync::{Arc, OnceLock};
use egui::{Style, Visuals};

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub enum Themes {
    Dark,
    Light,
}

static DARK_THEME: OnceLock<Arc<Style>> = OnceLock::new();
static LIGHT_THEME: OnceLock<Arc<Style>> = OnceLock::new();

pub fn get_theme(theme: Themes) -> Arc<Style> {
    match theme {
        Themes::Dark => Arc::clone(DARK_THEME.get_or_init(|| {
            let mut style = Style {
                visuals: Visuals::dark(),
                ..Style::default()
            };

            style.visuals.widgets.active.rounding = (0.).into();
            style.visuals.widgets.inactive.rounding = (0.).into();
            style.visuals.widgets.hovered.rounding = (0.).into();

            style.visuals.selection.bg_fill = egui::Color32::DARK_GRAY;
            style.visuals.widgets.hovered.bg_fill = egui::Color32::DARK_GRAY;
            // style.visuals.widgets.hovered.bg_stroke.width = 0.;
            // style.visuals.widgets.active.bg_stroke.width = 0.;
            // style.visuals.widgets.hovered.expansion = 0.;

            style.visuals.widgets.active.fg_stroke.color = egui::Color32::WHITE;
            style.visuals.widgets.inactive.fg_stroke.color = egui::Color32::WHITE;
            style.visuals.selection.stroke.color = egui::Color32::WHITE;

            Arc::new(style)
        })),

        Themes::Light => Arc::clone(LIGHT_THEME.get_or_init(|| {
            let mut style = Style {
                visuals: Visuals::light(),
                ..Style::default()
            };

            style.visuals.widgets.active.rounding = (0.).into();
            style.visuals.widgets.inactive.rounding = (0.).into();
            style.visuals.widgets.hovered.rounding = (0.).into();

            // style.visuals.selection.bg_fill = egui::Color32::DARK_GRAY;
            // style.visuals.widgets.hovered.bg_fill = egui::Color32::DARK_GRAY;
            style.visuals.widgets.hovered.bg_stroke.width = 0.;
            style.visuals.widgets.active.bg_stroke.width = 0.;
            style.visuals.widgets.hovered.expansion = 0.;

            style.visuals.widgets.active.fg_stroke.color = egui::Color32::BLACK;
            style.visuals.widgets.inactive.fg_stroke.color = egui::Color32::BLACK;
            style.visuals.selection.stroke.color = egui::Color32::BLACK;

            Arc::new(style)
        }))
    }
}
