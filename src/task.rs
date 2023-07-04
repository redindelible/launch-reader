use std::panic;
use std::sync::{Arc, Mutex};
use std::thread::JoinHandle;

pub struct Task<T: Send + 'static> {
    handle: JoinHandle<T>
}

impl<T: Send + 'static> Task<T> {
    pub fn new<F>(f: F) -> Task<T> where F: FnOnce() -> T + 'static + Send {
        Task { handle: std::thread::spawn(f) }
    }

    pub fn is_ready(&self) -> bool {
        self.handle.is_finished()
    }

    pub fn result(self) -> T {
        match self.handle.join() {
            Ok(val) => val,
            Err(e) => panic::resume_unwind(e)
        }
    }
}

#[derive(Clone)]
pub struct Tracker {
    tracker: Arc<Mutex<f32>>
}

impl Tracker {
    pub fn set_progress(&self, progress: f32) {
        *self.tracker.lock().unwrap() = progress;
    }
}


pub struct ProgressTask<T: Send + 'static> {
    handle: JoinHandle<T>,
    progress_tracker: Tracker
}

impl<T: Send + 'static> ProgressTask<T> {
    pub fn new<F>(f: F) -> ProgressTask<T> where F: FnOnce(Tracker) -> T + 'static + Send {
        let tracker = Tracker { tracker: Arc::new(Mutex::new(0.0)) };
        ProgressTask { progress_tracker: tracker.clone(), handle: std::thread::spawn(move || f(tracker)) }
    }

    pub fn is_ready(&self) -> bool {
        self.handle.is_finished()
    }

    pub fn progress(&self) -> f32 {
        *self.progress_tracker.tracker.lock().unwrap()
    }

    pub fn result(self) -> T {
        match self.handle.join() {
            Ok(val) => val,
            Err(e) => panic::resume_unwind(e)
        }
    }
}