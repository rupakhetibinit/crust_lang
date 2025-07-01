use thiserror::Error;

#[derive(Error, Debug)]
pub enum ParserError {
    #[error("Unexpected token: expected {expected}, found {found}")]
    UnexpectedToken {
        expected: String,
        found: String,
        span: Option<std::ops::Range<usize>>,
    },

    #[error("Expected type annotations found {found}")]
    ExpectedTypeAnnotation { found: String },

    #[error("Expected identifier, found {found}")]
    ExpectedIdentifier { found: String },

    #[error("Expected '{expected}', found '{found}'")]
    ExpectedToken { expected: String, found: String },

    #[error("Expected opening parenthesis '(', found {found}")]
    ExpectedOpeningParen { found: String },

    #[error("Expected closing parenthesis ')', found {found}")]
    ExpectedClosingParen { found: String },

    #[error("Expected colon ':', found {found}")]
    ExpectedColon { found: String },

    #[error("Expected comma ',' or closing parenthesis ')', found {found}")]
    ExpectedCommaOrClosingParen { found: String },

    #[error("Expected opening brace '{{', found {found}")]
    ExpectedOpeningBrace { found: String },

    #[error("Expected closing brace '}}', found {found}")]
    ExpectedClosingBrace { found: String },

    #[error("Expected assignment operator '=', found {found}")]
    ExpectedAssignment { found: String },

    #[error("Unexpected end of input, expected {expected}")]
    UnexpectedEndOfInput { expected: String },

    #[error("Invalid expression starting with {token}")]
    InvalidExpression { token: String },

    #[error("Empty parameter list not allowed")]
    EmptyParameterList,

    #[error("Type error: {message}")]
    TypeError { message: String },
}

#[derive(Error, Debug)]
#[error("Parse error at line {line}, column {column}: {source}")]
pub struct LocatedParserError {
    pub line: usize,
    pub column: usize,
    pub span: Option<std::ops::Range<usize>>,
    #[source]
    pub source: ParserError,
}

impl LocatedParserError {
    pub fn new(line: usize, column: usize, source: ParserError) -> Self {
        Self {
            line,
            column,
            span: None,
            source,
        }
    }

    pub fn with_span(
        line: usize,
        column: usize,
        span: std::ops::Range<usize>,
        source: ParserError,
    ) -> Self {
        Self {
            line,
            column,
            span: Some(span),
            source,
        }
    }
}

impl LocatedParserError {
    pub fn display_with_source(&self, source: &str, filename: Option<&str>) -> String {
        let mut output = String::new();

        if let Some(filename) = filename {
            output.push_str(&format!(
                "{}:{}:{}: ",
                filename,
                self.line + 1,
                self.column + 1
            ));
        }

        output.push_str(&format!("error: {}\n", self.source));

        let lines: Vec<&str> = source.lines().collect();

        if self.line < lines.len() {
            output.push_str(&format!("    |\n"));
            output.push_str(&format!("{:3} | {}\n", self.line + 1, lines[self.line]));
            output.push_str(&format!("    | "));

            if let Some(span) = &self.span {
                let mut line_start = 0;
                let mut current_line = 0;

                for (i, ch) in source.char_indices() {
                    if current_line == self.line {
                        line_start = i;
                        break;
                    }
                    if ch == '\n' {
                        current_line += 1;
                    }
                }

                let span_start_in_line = span.start.saturating_sub(line_start);
                let span_end_in_line =
                    (span.end.saturating_sub(line_start)).min(lines[self.line].len());

                for _ in 0..span_start_in_line {
                    output.push(' ');
                }

                let span_len = span_end_in_line.saturating_sub(span_start_in_line).max(1);
                for _ in 0..span_len {
                    output.push('^');
                }
            } else {
                for _ in 0..self.column {
                    output.push(' ');
                }
                output.push('^');
            }
            output.push('\n');
        }

        output
    }
}

pub fn extract_span_from_error(error: &ParserError) -> Option<std::ops::Range<usize>> {
    match error {
        ParserError::UnexpectedToken { span, .. } => span.clone(),
        _ => None,
    }
}
