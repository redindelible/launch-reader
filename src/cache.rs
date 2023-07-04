#![allow(dead_code)]

use std::sync::{Arc, mpsc, OnceLock};
use std::path::PathBuf;
use directories::ProjectDirs;
use serde::{Serialize, de::DeserializeOwned};

pub trait OnceLockable : Send + Sync + 'static { }
impl<T> OnceLockable for T where T: Send + Sync + 'static { }

pub trait Cacheable : Serialize + DeserializeOwned + Default + OnceLockable { }
impl<T> Cacheable for T where T: Serialize + DeserializeOwned + Default + OnceLockable { }

enum CacheRequest<T: Cacheable> {
    Mutate {
        mutate_fn: Box<dyn for<'a> FnOnce(&'a mut T) + Send + 'static>
    },
    Access {
        access_fn: Box<dyn for<'a> FnOnce(&'a T) + Send + 'static>
    },
    Clear
}

pub struct Cache<T: Cacheable> {
    path: PathBuf,
    requests: mpsc::Receiver<CacheRequest<T>>
}

impl<T: Cacheable> Cache<T> {
    fn new() -> Option<(Cache<T>, CacheHandle<T>)> {
        let project_dir = ProjectDirs::from("org", "ISS", &format!("LaunchReader{}.{}", env!("CARGO_PKG_VERSION_MAJOR"), env!("CARGO_PKG_VERSION_MINOR")));
        let path = project_dir?.cache_dir().join("cache.toml");
        let (sender, receiver) = mpsc::channel();
        Some((Cache { path, requests: receiver}, CacheHandle { sender }))
    }

    pub fn start_caching() -> Option<CacheHandle<T>> {
        let (mut cache, handle) = Cache::new()?;
        std::thread::spawn(move || cache.run());
        Some(handle)
    }

    fn load_cache(&self) -> Option<T> {
        loop {
            match confy::load_path::<T>(&self.path) {
                Ok(cfg) => {
                    return Some(cfg);
                },
                Err(confy::ConfyError::BadTomlData(_)) => {
                    std::fs::remove_file(&self.path).ok();
                }
                _ => return None
            };
        }
    }

    fn run(&mut self) {
        let Some(mut cache) = self.load_cache() else {
            for _ in self.requests.iter() { }
            unreachable!();
        };

        for request in self.requests.iter() {
            match request {
                CacheRequest::Access { access_fn: f } => {
                    f(&cache);
                }
                CacheRequest::Mutate { mutate_fn: value } => {
                    value(&mut cache);
                    confy::store_path(&self.path, &cache).ok();
                }
                CacheRequest::Clear => {
                    std::fs::remove_file(&self.path).ok();
                }
            }
        }
    }
}

pub struct CacheHandle<T: Cacheable> {
    sender: mpsc::Sender<CacheRequest<T>>
}

impl<T: Cacheable> Clone for CacheHandle<T> {
    fn clone(&self) -> Self {
        CacheHandle { sender: self.sender.clone() }
    }
}

impl<T: Cacheable> CacheHandle<T> {
    pub fn new_disconnected() -> CacheHandle<T> {
        CacheHandle { sender: mpsc::channel().0 }
    }

    pub fn clear(&self) {
        self.sender.send(CacheRequest::Clear).ok();
    }

    pub fn update(&self, value: impl for<'a> FnOnce(&'a mut T) + 'static + Send) {
        self.sender.send(CacheRequest::Mutate { mutate_fn: Box::new(value) }).ok();
    }

    pub fn read<V: OnceLockable, F: for<'a> FnOnce(&'a T) -> V + 'static + Send>(&self, f: F) -> AccessRequest<V> {
        let storage: Arc<OnceLock<V>> = Arc::new(OnceLock::new());
        let sent_storage = Arc::clone(&storage);
        self.sender.send(CacheRequest::Access {
            access_fn: Box::new(move |v| { sent_storage.set(f(v)).ok(); })
        }).ok();
        AccessRequest { result: storage }
    }
}


#[derive(Clone)]
pub struct AccessRequest<T: OnceLockable> {
    result: Arc<OnceLock<T>>
}

impl<T: OnceLockable> AccessRequest<T> {
    pub fn is_ready(&self) -> bool {
        self.result.get().is_some()
    }

    pub fn fetch(&self) -> Option<&T> {
        self.result.get()
    }
}
