use ariadne::{Color, Label, Report, ReportKind, Source};

use crate::{
    lexer::Token,
    parser::{ParseError, ParseErrorKind},
};
use std::ops::Range;

pub fn report_parse_error(source: &str, error: &ParseError) {
    let mut report = Report::build(ReportKind::Error, (&error.filename, error.span.clone()))
        .with_message(get_error_message(&error.kind));

    match &error.kind {
        ParseErrorKind::UnexpectedToken { found, expected } => {
            let found_str = format!("{:?}", found);
            let expected_str = expected
                .as_ref()
                .map(|s| s.as_str())
                .unwrap_or("something else");

            report = report.with_label(
                Label::new((&error.filename, error.span.clone()))
                    .with_message(format!(
                        "found {}, but expected {}",
                        found_str, expected_str
                    ))
                    .with_color(Color::Red),
            );

            // Add suggestion if it's a common mistake
            if let Some(suggestion) = suggest_token_fix(&found_str, expected.as_ref()) {
                report = report.with_note(format!("{}", suggestion));
            }
        }

        ParseErrorKind::UnexpectedEOF { expected } => {
            let expected_str = expected
                .as_ref()
                .map(|s| s.as_str())
                .unwrap_or("more input");

            report = report.with_label(
                Label::new((&error.filename, error.span.clone()))
                    .with_message(format!("unexpected end of file, expected {}", expected_str))
                    .with_color(Color::Red),
            );

            report = report.with_note("check if you're missing a closing delimiter or statement");
        }

        ParseErrorKind::UnclosedBlock { open_span } => {
            report = report.with_label(
                Label::new((&error.filename, error.span.clone()))
                    .with_message("unclosed block found here")
                    .with_color(Color::Red),
            );

            report = report.with_label(
                Label::new((&error.filename, open_span.clone()))
                    .with_message("block opened here")
                    .with_color(Color::Blue),
            );

            report = report.with_note("add a closing `}` to match the opening brace");
        }

        ParseErrorKind::ExpectedFunctionName {} => {
            report = report.with_label(
                Label::new((&error.filename, error.span.clone()))
                    .with_message("expected function name after `fn` keyword")
                    .with_color(Color::Red),
            );

            report = report.with_note(
                "function declarations must have a name, like `fn my_function() { ... }`",
            );
        }

        ParseErrorKind::InvalidType { found } => {
            let found_str = format!("{:?}", found);

            report = report.with_label(
                Label::new((&error.filename, error.span.clone()))
                    .with_message(format!("{} is not a valid type", found_str))
                    .with_color(Color::Red),
            );

            // Suggest similar valid types
            if let Some(suggestion) = suggest_similar_type(&found_str) {
                report = report.with_note(format!("help: did you mean `{}`?", suggestion));
            } else {
                report =
                    report.with_note("valid types include `int`, `string`, `bool`, `float`, etc.");
            }
        }

        ParseErrorKind::ExpectedIdentAfterLet => {
            report = report.with_label(
                Label::new((&error.filename, error.span.clone()))
                    .with_message("expected variable name after `let`")
                    .with_color(Color::Red),
            );

            report = report
                .with_note("variable declarations must have a name, like `let my_var = value;`");
        }
    }

    report
        .finish()
        .print((&error.filename, Source::from(source)))
        .unwrap();
}

// Get error message for each error kind
fn get_error_message(kind: &ParseErrorKind) -> String {
    match kind {
        ParseErrorKind::UnexpectedToken { .. } => "Unexpected token".to_string(),
        ParseErrorKind::UnexpectedEOF { .. } => "Unexpected end of file".to_string(),
        ParseErrorKind::UnclosedBlock { .. } => "Unclosed block".to_string(),
        ParseErrorKind::ExpectedFunctionName {} => "Expected function name".to_string(),
        ParseErrorKind::InvalidType { .. } => "Invalid type".to_string(),
        ParseErrorKind::ExpectedIdentAfterLet => "Expected identifier after let".to_string(),
    }
}

// Helper function to suggest fixes for common token mistakes
fn suggest_token_fix(found_str: &str, expected: Option<&String>) -> Option<String> {
    let expected_str = expected.map(|s| s.as_str()).unwrap_or("");

    // Common mistake mappings based on string representation
    if found_str.contains("Equal") && expected_str.contains("==") {
        return Some("use `==` for comparison, `=` is for assignment".to_string());
    }

    // Common mistake mappings based on string representation
    if found_str.contains("EqualEqual") && expected_str.contains("=") {
        return Some("use `==` for comparison, `=` is for assignment".to_string());
    }

    if found_str.contains("LBrace") && expected_str.contains("(") {
        return Some("use `(` for function parameters, `{` for blocks".to_string());
    }

    if found_str.contains("LParen") && expected_str.contains("{") {
        return Some("use `{` to start a block".to_string());
    }

    if found_str.contains("EOF") {
        return Some(
            "the file ended unexpectedly - check for missing closing delimiters".to_string(),
        );
    }

    None
}

// Helper function to suggest similar valid types
fn suggest_similar_type(found: &str) -> Option<String> {
    let valid_types = ["int", "string", "bool", "float", "char", "void"];

    // Extract potential type name from debug string
    let cleaned = found
        .trim_matches(|c| c == '(' || c == ')' || c == '"')
        .to_lowercase();

    // Simple fuzzy matching - find the type with the smallest edit distance
    let mut best_match = None;
    let mut best_distance = usize::MAX;

    for valid_type in &valid_types {
        let distance = edit_distance(&cleaned, valid_type);
        if distance < best_distance && distance <= 2 {
            // Only suggest if reasonably close
            best_distance = distance;
            best_match = Some(valid_type.to_string());
        }
    }

    best_match
}

// Simple edit distance calculation for type suggestions
fn edit_distance(a: &str, b: &str) -> usize {
    let a_chars: Vec<char> = a.chars().collect();
    let b_chars: Vec<char> = b.chars().collect();
    let mut matrix = vec![vec![0; b_chars.len() + 1]; a_chars.len() + 1];

    // Initialize first row and column
    for i in 0..=a_chars.len() {
        matrix[i][0] = i;
    }
    for j in 0..=b_chars.len() {
        matrix[0][j] = j;
    }

    // Fill the matrix
    for i in 1..=a_chars.len() {
        for j in 1..=b_chars.len() {
            let cost = if a_chars[i - 1] == b_chars[j - 1] {
                0
            } else {
                1
            };
            matrix[i][j] = std::cmp::min(
                std::cmp::min(matrix[i - 1][j] + 1, matrix[i][j - 1] + 1),
                matrix[i - 1][j - 1] + cost,
            );
        }
    }

    matrix[a_chars.len()][b_chars.len()]
}

// Convenience constructors for your ParseError struct
impl ParseError {
    pub fn unexpected_token(
        filename: String,
        span: Range<usize>,
        found: Token,
        expected: Option<String>,
    ) -> Self {
        Self {
            filename,
            span,
            kind: ParseErrorKind::UnexpectedToken { found, expected },
        }
    }

    pub fn unexpected_eof(filename: String, span: Range<usize>, expected: Option<String>) -> Self {
        Self {
            filename,
            span,
            kind: ParseErrorKind::UnexpectedEOF { expected },
        }
    }

    pub fn unclosed_block(filename: String, span: Range<usize>, open_span: Range<usize>) -> Self {
        Self {
            filename,
            span,
            kind: ParseErrorKind::UnclosedBlock { open_span },
        }
    }

    pub fn expected_function_name(filename: String, span: Range<usize>) -> Self {
        Self {
            filename,
            span,
            kind: ParseErrorKind::ExpectedFunctionName {},
        }
    }

    pub fn invalid_type(filename: String, span: Range<usize>, found: Token) -> Self {
        Self {
            filename,
            span,
            kind: ParseErrorKind::InvalidType { found },
        }
    }

    pub fn expected_ident_after_let(filename: String, span: Range<usize>) -> Self {
        Self {
            filename,
            span,
            kind: ParseErrorKind::ExpectedIdentAfterLet,
        }
    }
}
