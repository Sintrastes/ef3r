use bimap::BiMap;
use ef3r::{
    debugging::NoOpDebugger, executable::load_efrs_source,
    lsp::autocomplete::autocomplete,
};

#[test]
fn test_list_autocomplete() {
    let (context, statements) =
        load_efrs_source(NoOpDebugger {}, "list(1, 2, 3)".to_string()).unwrap();

    let expr_to_complete = statements[0].expr.clone();

    let completions = autocomplete(&context, &expr_to_complete);

    println!("Completions were: {:?}", completions);

    // List methods should be included in completions
    assert!(completions.contains(&"map".to_string()));
    assert!(completions.contains(&"filter".to_string()));
    assert!(completions.contains(&"fold".to_string()));
}
