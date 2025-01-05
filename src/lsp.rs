pub mod autocomplete;

use std::sync::{Arc, Mutex};

use tower_lsp::{jsonrpc::Result, lsp_types::*, Client, LspService, Server};

use crate::{
    debugging::NoOpDebugger, frp::with_lock, interpreter::Context,
    stdlib::ef3r_stdlib,
};

pub struct LspBackend {
    client: Client,
    context: Arc<Mutex<Context<'static, NoOpDebugger>>>,
}

impl LspBackend {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            context: Arc::new(Mutex::new(ef3r_stdlib(
                NoOpDebugger::new(),
                bimap::BiMap::new(),
            ))),
        }
    }
}

#[tower_lsp::async_trait]
impl tower_lsp::LanguageServer for LspBackend {
    async fn initialize(
        &self,
        _: InitializeParams,
    ) -> Result<InitializeResult> {
        Ok(InitializeResult {
            server_info: None,
            capabilities: ServerCapabilities {
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(false),
                    trigger_characters: Some(vec![".".to_string()]),
                    all_commit_characters: None,
                    work_done_progress_options: Default::default(),
                    completion_item: None,
                }),
                ..ServerCapabilities::default()
            },
        })
    }

    async fn initialized(&self, _: InitializedParams) {}

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn completion(
        &self,
        params: CompletionParams,
    ) -> Result<Option<CompletionResponse>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;

        // Read document content
        let content = std::fs::read_to_string(uri.path()).unwrap();
        let lines: Vec<&str> = content.lines().collect();

        // Extract current line up to cursor
        let current_line = lines
            .get(position.line as usize)
            .map(|line| &line[..position.character as usize])
            .unwrap_or("");

        // Get completions
        let completions = with_lock(self.context.as_ref(), |context| {
            autocomplete::autocomplete(context, current_line)
        });

        Ok(Some(CompletionResponse::Array(
            completions
                .into_iter()
                .map(|item| CompletionItem {
                    label: item,
                    kind: Some(CompletionItemKind::FUNCTION),
                    detail: None,
                    ..CompletionItem::default()
                })
                .collect(),
        )))
    }
}

pub async fn start_lsp() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| LspBackend::new(client));
    Server::new(stdin, stdout, socket).serve(service).await;
}
