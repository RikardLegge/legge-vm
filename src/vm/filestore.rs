use crate::Path;
use std::collections::HashMap;
use std::result;
use tokio::io::{AsyncReadExt, ErrorKind};

pub type Result = result::Result<String, Err>;

pub trait FileStore {
    fn file(&self, path: &Path) -> File;
}

pub struct SystemFileStore {}

impl SystemFileStore {
    pub fn new() -> Self {
        Self {}
    }
}

impl FileStore for SystemFileStore {
    fn file(&self, path: &Path) -> File {
        let content = FileReaderContent::Async(path.clone());
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
    fn file(&self, path: &Path) -> File {
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
    Async(Path),
    Static(String),
    Err(Err),
}

pub struct File {
    content: FileReaderContent,
}

impl File {
    pub async fn read(&self) -> Result {
        match &self.content {
            FileReaderContent::Async(path) => {
                match tokio::fs::File::open(format!("{}.bc", path.first())).await {
                    Ok(mut file) => {
                        let mut code = String::new();
                        file.read_to_string(&mut code)
                            .await
                            .expect("something went wrong reading file");
                        Ok(code)
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
                            details: err.to_string(),
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
