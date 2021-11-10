use crate::Path;
use std::collections::HashMap;
use std::path::PathBuf;
use std::result;
use tokio::io::{AsyncReadExt, ErrorKind};

pub type Result = result::Result<String, Err>;

pub trait FileStore {
    fn file(&self, root: PathBuf, path: &Path) -> File;
}

pub struct SystemFileStore {}

impl SystemFileStore {
    pub fn new() -> Self {
        Self {}
    }
}

impl FileStore for SystemFileStore {
    fn file(&self, root: PathBuf, path: &Path) -> File {
        let content = FileReaderContent::Async(root, path.clone());
        File { content }
    }
}

pub struct VirtualFileStore {
    files: HashMap<Path, String>,
}

impl VirtualFileStore {
    pub fn new() -> Self {
        Self {
            files: HashMap::new(),
        }
    }

    pub fn add(&mut self, path: Path, content: String) {
        self.files.insert(path, content);
    }
}

impl FileStore for VirtualFileStore {
    fn file(&self, _: PathBuf, path: &Path) -> File {
        let content = match self.files.get(path) {
            Some(content) => FileReaderContent::Static(content.clone()),
            None => FileReaderContent::Err(Err {
                retryable: false,
                details: format!("File not found: {:?}", path),
            }),
        };
        File { content }
    }
}

#[derive(Debug, Clone)]
pub struct Err {
    details: String,
    retryable: bool,
}

impl ToString for Err {
    fn to_string(&self) -> String {
        self.details.clone()
    }
}

impl Err {
    pub fn retryable(&self) -> bool {
        self.retryable
    }
}

enum FileReaderContent {
    Async(PathBuf, Path),
    Static(String),
    Err(Err),
}

pub struct File {
    content: FileReaderContent,
}

impl File {
    pub async fn read(&self) -> Result {
        match &self.content {
            FileReaderContent::Async(root, path) => {
                let mut file_name = root.clone();
                for part in path.as_ref().as_ref() {
                    file_name.push(part.clone());
                }
                file_name.set_extension("bc");

                match tokio::fs::File::open(&file_name).await {
                    Ok(mut file) => {
                        let mut code = String::new();
                        match file.read_to_string(&mut code).await {
                            Ok(_) => Ok(code),
                            Err(err) => Err(Err {
                                retryable: false,
                                details: format!(
                                    "Failed reading file {}: {}",
                                    file_name.to_str().unwrap(),
                                    err
                                ),
                            }),
                        }
                    }
                    Err(err) => match err.kind() {
                        ErrorKind::NotFound
                        | ErrorKind::PermissionDenied
                        | ErrorKind::BrokenPipe
                        | ErrorKind::TimedOut
                        | ErrorKind::Interrupted
                        | ErrorKind::Unsupported
                        | ErrorKind::Other => Err(Err {
                            retryable: false,
                            details: format!(
                                "Failed reading file {}: {}",
                                file_name.to_str().unwrap(),
                                err
                            ),
                        }),
                        _ => Err(Err {
                            retryable: true,
                            details: String::new(),
                        }),
                    },
                }
            }
            FileReaderContent::Static(content) => Ok(content.clone()),
            FileReaderContent::Err(err) => Err(err.clone()),
        }
    }
}
