pub mod autocomplete;

use std::sync::{Arc, RwLock};

use tower_lsp::{jsonrpc::Result, lsp_types::*, Client, LspService, Server};

use crate::{debugging::NoOpDebugger, executable::load_efrs_source};

pub struct LspBackend {
    client: Client,
}

impl LspBackend {
    pub fn new(client: Client) -> Self {
        Self { client }
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

        // Interpret the source.
        let (context, statements) =
            load_efrs_source(NoOpDebugger {}, content).unwrap();

        // Get completions
        let completions =
            autocomplete::autocomplete_at(&context, &statements, position);

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
