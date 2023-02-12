//! TOML subset parser
//!
//! Supports multi-line table literals trailing commas everywhere. Because why the hell was that
//! not a part of TOML in the first place? C'mon Tom. That's not obvious.

use indexmap::IndexMap;
use std::collections::VecDeque;

#[derive(Debug)]
pub enum BlackDwarfError {
    UnknownKey {
        key: String,
        where_: Pos,
    },

    UnknownFileGroup {
        what: String,
        where_: Pos,
    },

    MissingKey {
        key: &'static str,
        where_: Pos,
    },

    IncorrectType {
        type_: &'static str,
        expected: &'static str,
        where_: Pos,
    },

    ParseError {
        why: String,
        where_: Pos,
    },
}

pub enum Value<'doc> {
    Table {
        key_values: IndexMap<&'doc str, Value<'doc>>,
        pos: Pos,
    },

    Array {
        values: Vec<Value<'doc>>,
        pos: Pos,
    },

    String {
        value: &'doc str,
        pos: Pos,
    },

    Integer {
        value: i64,
        pos: Pos,
    },

    Float {
        value: f64,
        pos: Pos,
    },

    Boolean {
        value: bool,
        pos: Pos,
    },

    Datetime {
        datetime: Datetime,
        pos: Pos,
    },
}

#[derive(PartialEq, Clone, Copy)]
pub struct Datetime {
    pub date: Option<Date>,
    pub time: Option<Time>,
    pub offset: Option<Offset>,
}

impl ToString for Datetime {
    fn to_string(&self) -> String {
        let mut s = String::new();

        if let Some(date) = self.date {
            s += &format!("{:04}-{:02}-{:02}", date.year, date.month, date.day);
            if self.time.is_some() {
                s += "T";
            }
        }

        if let Some(time) = self.time {
            s += &format!(
                "{:02}:{:02}:{:02}.{:.03}",
                time.hour,
                time.minute,
                time.second,
                time.nanosecond as f32 / 1_000_000_000.0
            );
        }

        if let Some(Offset::Z) = self.offset {
            s += &format!("Z");
        } else if let Some(Offset::Minutes(signed_minutes)) = self.offset {
            let minutes = if signed_minutes.is_negative() {
                s += "-";
                -signed_minutes
            } else {
                s += "+";
                signed_minutes
            };
            let hours = minutes as f64 / 60.;
            let hours_trunc = hours as u16;
            let minutes_trunc = (hours - hours_trunc as f64) * 60.;
            s += &format!("{:02}:{:02}", hours_trunc, minutes_trunc);
        }

        s
    }
}

impl std::fmt::Debug for Datetime {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut debug_struct = f.debug_struct("Datetime");

        if let Some(date) = self.date.as_ref() {
            debug_struct.field("year", &date.year);
            debug_struct.field("month", &date.month);
            debug_struct.field("day", &date.day);
        }

        if let Some(time) = self.time.as_ref() {
            debug_struct.field("hour", &time.hour);
            debug_struct.field("minute", &time.minute);
            debug_struct.field("second", &time.second);
            debug_struct.field("nanosecond", &time.nanosecond);
        }

        if let Some(offset) = self.offset.as_ref() {
            debug_struct.field("offset", &offset);
        }

        debug_struct.finish()
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Date {
    pub year: u16,
    pub month: u8,
    pub day: u8,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Time {
    pub hour: u8,
    pub minute: u8,
    pub second: u8,
    pub nanosecond: u32,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Offset {
    Z,
    Minutes(i16),
}

impl<'doc> Value<'doc> {
    fn new_table(pos: Pos) -> Self {
        Value::Table {
            key_values: IndexMap::new(),
            pos,
        }
    }

    fn new_array(pos: Pos) -> Self {
        Value::Array {
            values: vec![],
            pos,
        }
    }

    pub fn type_str(&self) -> &'static str {
        match self {
            Value::Array { .. } => "array",
            Value::Table { .. } => "table",
            Value::String { .. } => "string",
            Value::Integer { .. } => "integer",
            Value::Float { .. } => "float",
            Value::Boolean { .. } => "boolean",
            Value::Datetime { .. } => "datetime",
        }
    }

    pub fn pos(&self) -> Pos {
        match self {
            Value::Array { pos, .. } => *pos,
            Value::Table { pos, .. } => *pos,
            Value::String { pos, .. } => *pos,
            Value::Integer { pos, .. } => *pos,
            Value::Float { pos, .. } => *pos,
            Value::Boolean { pos, .. } => *pos,
            Value::Datetime { pos, .. } => *pos,
        }
    }

    pub fn get_mut(&mut self, key: &str) -> Option<&mut Value<'doc>> {
        match self {
            Value::Table { key_values, .. } => key_values.get_mut(key),
            _ => None,
        }
    }

    pub fn get(&self, key: &str) -> Option<&Value<'doc>> {
        match self {
            Value::Table { key_values, .. } => key_values.get(key),
            _ => None,
        }
    }

    pub fn contains_key(&self, key: &str) -> bool {
        match self {
            Value::Table { key_values, .. } => key_values.contains_key(key),
            _ => false,
        }
    }

    pub fn is_str(&self) -> bool {
        matches!(self, Value::String { .. })
    }

    pub fn is_table(&self) -> bool {
        matches!(self, Value::Table { .. })
    }

    pub fn is_array(&self) -> bool {
        matches!(self, Value::Array { .. })
    }

    pub fn is_just_date(&self) -> bool {
        matches!(
            self,
            Value::Datetime {
                datetime: Datetime {
                    date: Some(_),
                    time: None,
                    ..
                },
                ..
            }
        )
    }

    pub fn as_list(&self) -> Option<&[Value]> {
        match self {
            Value::Array { values, .. } => Some(&values),
            _ => None,
        }
    }

    pub fn as_list_mut(&mut self) -> Option<&mut Vec<Value<'doc>>> {
        match self {
            Value::Array { values, .. } => Some(values),
            _ => None,
        }
    }

    pub fn as_str(&self) -> Option<&'doc str> {
        match self {
            Value::String { value, .. } => Some(value),
            _ => None,
        }
    }

    pub fn iter_kvs(&self) -> TableIter {
        TableIter {
            inner: match self {
                Value::Table { key_values, .. } => Some(key_values.iter()),
                _ => None,
            },
        }
    }

    fn insert(&mut self, key: &'doc str, value: Value<'doc>) {
        match self {
            Value::Table { key_values, .. } => {
                key_values.insert(key, value);
            }
            _ => panic!("called create on non-table"),
        }
    }

    fn append(&mut self, value: Value<'doc>) {
        match self {
            Value::Array { values, .. } => values.push(value),
            _ => panic!("called append on non-array"),
        }
    }
}

impl std::fmt::Debug for Value<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Table { key_values, .. } => f.debug_map().entries(key_values.iter()).finish(),
            Value::Array { values, .. } => f.debug_list().entries(values.iter()).finish(),
            Value::String { value, .. } => value.fmt(f),
            Value::Integer { value, .. } => value.fmt(f),
            Value::Float { value, .. } => value.fmt(f),
            Value::Boolean { value, .. } => value.fmt(f),
            Value::Datetime { datetime, .. } => datetime.fmt(f),
        }
    }
}

// memory safety 👍
pub struct TableIter<'table, 'doc: 'table> {
    inner: Option<indexmap::map::Iter<'table, &'doc str, Value<'doc>>>,
}

impl<'table, 'doc: 'table> Iterator for TableIter<'table, 'doc> {
    type Item = (&'doc str, &'table Value<'doc>);
    fn next(&mut self) -> Option<Self::Item> {
        self.inner.as_mut()?.next().map(|(k, v)| (*k, v))
    }
}

pub fn scan(doc: &str) -> Result<Vec<Token>, BlackDwarfError> {
    Scanner::new(doc).scan_all()
}

pub fn parse(doc: &str) -> Result<Value, BlackDwarfError> {
    let mut scanner = Scanner::new(doc);
    let first = scanner.peek_token(0)?.pos;

    let mut top_level = Value::new_table(first);
    while scanner.peek_token(0)?.type_ != TokenType::Eof {
        let peeked = scanner.peek_token(0)?;
        if peeked.type_.may_be_key() {
            parse_kv(&mut scanner, &mut top_level, 0)?;
        } else if peeked.type_ == TokenType::LeftBracket {
            if scanner.peek_token(1)?.type_ == TokenType::LeftBracket {
                parse_multiline_array_element(&mut scanner, &mut top_level, 0)?;
            } else {
                parse_multiline_table(&mut scanner, &mut top_level, 0)?;
            }
        } else {
            return Err(BlackDwarfError::ParseError {
                why: format!("expected key or table header, got '{}'", peeked.lexeme),
                where_: peeked.pos,
            });
        }
    }

    Ok(top_level)
}

macro_rules! ensure {
    ($depth:ident, $scanner:ident) => {
        if $depth > 64 {
            return Err(BlackDwarfError::ParseError {
                why: format!("recursion limit exceeded"),
                where_: $scanner.peek_token(0)?.pos,
            });
        }

        let $depth = $depth + 1;
    };
}

fn parse_kv<'doc>(
    scanner: &mut Scanner<'doc>,
    mut current: &mut Value<'doc>,
    depth: usize,
) -> Result<(), BlackDwarfError> {
    ensure!(depth, scanner);

    let path = parse_path(scanner)?;
    let _equals = consume(scanner, TokenType::Equals)?;
    let mut value = parse_value(scanner, depth)?;

    // ew lol
    if scanner.peek_token(0)?.type_.is_time() {
        if let Value::Datetime {
            datetime:
                Datetime {
                    date: Some(date),
                    time: None,
                    ..
                },
            pos,
        } = value
        {
            let Token {
                type_: TokenType::Time {
                    time,
                    offset,
                },
                ..
            } = scanner.next_token()? else {
                unreachable!()
            };

            value = Value::Datetime {
                datetime: Datetime {
                    date: Some(date),
                    time: Some(time),
                    offset,
                },
                pos,
            };
        }
    }

    for (i, fragment) in path.iter().enumerate() {
        if !current.is_table() {
            return Err(BlackDwarfError::IncorrectType {
                type_: current.type_str(),
                expected: "table",
                where_: _equals.pos,
            });
        }

        if i + 1 != path.len() {
            if !current.contains_key(fragment.lexeme) {
                current.insert(fragment.lexeme, Value::new_table(fragment.pos));
            }

            current = current.get_mut(fragment.lexeme).unwrap();
        } else {
            current.insert(fragment.lexeme, value);
            break;
        }
    }

    Ok(())
}

fn parse_value<'doc>(
    scanner: &mut Scanner<'doc>,
    depth: usize,
) -> Result<Value<'doc>, BlackDwarfError> {
    ensure!(depth, scanner);
    let next = scanner.next_token()?;

    match next.type_ {
        TokenType::LeftBracket => parse_array(scanner, depth),

        TokenType::LeftBrace => parse_table(scanner, depth),

        TokenType::String => Ok(Value::String {
            value: &next.lexeme[1..next.lexeme.len() - 1],
            pos: next.pos,
        }),

        TokenType::Integer(value) => Ok(Value::Integer {
            value,
            pos: next.pos,
        }),

        TokenType::Float(value) => Ok(Value::Float {
            value,
            pos: next.pos,
        }),

        TokenType::Boolean(value) => Ok(Value::Boolean {
            value,
            pos: next.pos,
        }),

        TokenType::Time { time, offset } => Ok(Value::Datetime {
            datetime: Datetime {
                date: None,
                time: Some(time),
                offset,
            },
            pos: next.pos,
        }),

        TokenType::Date(date) => Ok(Value::Datetime {
            datetime: Datetime {
                date: Some(date),
                time: None,
                offset: None,
            },
            pos: next.pos,
        }),

        TokenType::Datetime(datetime) => Ok(Value::Datetime {
            datetime,
            pos: next.pos,
        }),

        _ => {
            return Err(BlackDwarfError::ParseError {
                why: format!("not yet supported: {:?}", next),
                where_: next.pos,
            })
        }
    }
}

fn parse_array<'doc>(
    scanner: &mut Scanner<'doc>,
    depth: usize,
) -> Result<Value<'doc>, BlackDwarfError> {
    ensure!(depth, scanner);
    let pos = scanner.peek_token(0)?.pos;
    if scanner.peek_token(0)?.type_ == TokenType::RightBracket {
        let _rb = consume(scanner, TokenType::RightBracket)?;
        return Ok(Value::Array {
            values: vec![],
            pos,
        });
    }

    let mut values = vec![parse_value(scanner, depth)?];
    while scanner.peek_token(0)?.type_ == TokenType::Comma && !scanner.is_at_end() {
        let _comma = consume(scanner, TokenType::Comma)?;
        if scanner.peek_token(0)?.type_ == TokenType::RightBracket {
            break;
        }
        values.push(parse_value(scanner, depth)?);
    }

    let _rb = consume(scanner, TokenType::RightBracket)?;
    Ok(Value::Array { values, pos })
}

fn parse_table<'doc>(
    scanner: &mut Scanner<'doc>,
    depth: usize,
) -> Result<Value<'doc>, BlackDwarfError> {
    ensure!(depth, scanner);
    let pos = scanner.peek_token(0)?.pos;
    if scanner.peek_token(0)?.type_ == TokenType::RightBrace {
        let _rb = consume(scanner, TokenType::RightBrace)?;
        return Ok(Value::Table {
            key_values: IndexMap::new(),
            pos,
        });
    }

    let mut key_values = Value::new_table(pos);
    parse_kv(scanner, &mut key_values, depth)?;
    while scanner.peek_token(0)?.type_ == TokenType::Comma && !scanner.is_at_end() {
        let _comma = consume(scanner, TokenType::Comma);
        if scanner.peek_token(0)?.type_ == TokenType::RightBrace {
            break;
        }
        parse_kv(scanner, &mut key_values, depth)?;
    }

    let _rb = consume(scanner, TokenType::RightBrace)?;
    Ok(key_values)
}

fn parse_multiline_table<'doc>(
    scanner: &mut Scanner<'doc>,
    top_level: &mut Value<'doc>,
    depth: usize,
) -> Result<(), BlackDwarfError> {
    ensure!(depth, scanner);
    let _lb = consume(scanner, TokenType::LeftBracket)?;
    let path = parse_path(scanner)?;
    let _rb = consume(scanner, TokenType::RightBracket)?;

    let mut current = &mut *top_level;
    for fragment in path.iter() {
        if current.is_array() {
            let type_ = current.type_str();
            current = current.as_list_mut().unwrap().last_mut().ok_or_else(|| {
                BlackDwarfError::IncorrectType {
                    type_,
                    expected: "array",
                    where_: fragment.pos,
                }
            })?;
        } else if !current.is_table() {
            return Err(BlackDwarfError::IncorrectType {
                type_: current.type_str(),
                expected: "table",
                where_: fragment.pos,
            });
        }

        if !current.contains_key(fragment.lexeme) {
            current.insert(fragment.lexeme, Value::new_table(fragment.pos));
        }

        current = current.get_mut(fragment.lexeme).unwrap();
    }

    while !scanner.peek_token(0)?.type_.is_bracket() && !scanner.is_at_end() {
        parse_kv(scanner, current, depth)?;
    }

    Ok(())
}

fn parse_multiline_array_element<'doc>(
    scanner: &mut Scanner<'doc>,
    top_level: &mut Value<'doc>,
    depth: usize,
) -> Result<(), BlackDwarfError> {
    ensure!(depth, scanner);
    let _lb = consume(scanner, TokenType::LeftBracket)?;
    let _lb = consume(scanner, TokenType::LeftBracket)?;
    let path = parse_path(scanner)?;
    let _rb = consume(scanner, TokenType::RightBracket)?;
    let _rb = consume(scanner, TokenType::RightBracket)?;

    let mut current = &mut *top_level;
    for (i, fragment) in path.iter().enumerate() {
        if current.is_array() {
            current = current
                .as_list_mut()
                .unwrap()
                .last_mut()
                .expect("unreachable?");
        } else if !current.is_table() {
            return Err(BlackDwarfError::IncorrectType {
                type_: current.type_str(),
                expected: "table",
                where_: fragment.pos,
            });
        }

        if !current.contains_key(fragment.lexeme) {
            if i + 1 == path.len() {
                current.insert(fragment.lexeme, Value::new_array(fragment.pos));
            } else {
                current.insert(fragment.lexeme, Value::new_table(fragment.pos));
            }
        }

        current = current.get_mut(fragment.lexeme).unwrap();
    }

    if !current.is_array() {
        return Err(BlackDwarfError::IncorrectType {
            type_: current.type_str(),
            expected: "array",
            where_: _lb.pos,
        });
    }

    let mut table = Value::new_table(scanner.peek_token(0)?.pos);
    while !scanner.peek_token(0)?.type_.is_bracket() && !scanner.is_at_end() {
        parse_kv(scanner, &mut table, depth)?;
    }
    current.append(table);

    Ok(())
}

fn parse_path<'doc>(scanner: &mut Scanner<'doc>) -> Result<Vec<Token<'doc>>, BlackDwarfError> {
    let mut names = vec![consume_key(scanner)?];
    while (scanner.peek_token(0)?.type_.may_be_key()
        || scanner.peek_token(0)?.type_ == TokenType::Dot)
        && !scanner.is_at_end()
    {
        let _dot = consume(scanner, TokenType::Dot)?;
        names.push(consume_key(scanner)?);
    }
    Ok(names)
}

fn consume_key<'doc>(scanner: &mut Scanner<'doc>) -> Result<Token<'doc>, BlackDwarfError> {
    let tok = scanner.next_token()?;
    if tok.type_.may_be_key() {
        Ok(tok)
    } else {
        Err(BlackDwarfError::ParseError {
            why: format!("expected non-symbol for key name, got '{}'", tok.lexeme),
            where_: tok.pos,
        })
    }
}

fn consume<'doc>(
    scanner: &mut Scanner<'doc>,
    type_: TokenType,
) -> Result<Token<'doc>, BlackDwarfError> {
    let tok = scanner.next_token()?;
    if tok.type_ == type_ {
        Ok(tok)
    } else {
        Err(BlackDwarfError::ParseError {
            why: format!("expected {:?}, got '{}'", type_, tok.lexeme),
            where_: tok.pos,
        })
    }
}

impl From<BlackDwarfError> for Vec<BlackDwarfError> {
    fn from(value: BlackDwarfError) -> Self {
        vec![value]
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Pos {
    line: usize,
    col: usize,
}

impl Pos {
    fn inc_line(&mut self) {
        self.line += 1;
    }

    fn inc_col(&mut self) {
        self.col += 1;
    }

    fn reset_col(&mut self) {
        self.col = 1;
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Token<'doc> {
    pub lexeme: &'doc str,
    pub type_: TokenType,
    pub pos: Pos,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenType {
    Integer(i64),
    Float(f64),
    Boolean(bool),
    String,
    Datetime(Datetime),
    Date(Date),
    Time {
        time: Time,
        offset: Option<Offset>,
    },

    Ident,

    /// [
    LeftBracket,

    /// ]
    RightBracket,

    /// {
    LeftBrace,

    /// }
    RightBrace,

    Equals,
    Dot,
    Comma,
    Eof,
}

impl TokenType {
    fn is_time(&self) -> bool {
        matches!(self, TokenType::Time { .. })
    }

    fn is_bracket(&self) -> bool {
        matches!(self, TokenType::LeftBracket)
    }

    fn may_be_key(&self) -> bool {
        !matches!(
            self,
            TokenType::LeftBracket
                | TokenType::RightBracket
                | TokenType::LeftBrace
                | TokenType::RightBrace
                | TokenType::Equals
                | TokenType::Dot
                | TokenType::Comma
                | TokenType::Eof
        )
    }
}

#[derive(Debug)]
struct Scanner<'a> {
    source: &'a [u8],
    tokens: VecDeque<Token<'a>>,
    start: usize,
    current: usize,
    start_pos: Pos,
    current_pos: Pos,
}

impl<'a> Scanner<'a> {
    fn new(source: &'a str) -> Self {
        Scanner {
            source: source.as_bytes(),
            tokens: VecDeque::new(),
            start: 0,
            current: 0,
            start_pos: Pos { line: 1, col: 1 },
            current_pos: Pos { line: 1, col: 1 },
        }
    }

    fn scan_all(mut self) -> Result<Vec<Token<'a>>, BlackDwarfError> {
        while self.next()?.type_ != TokenType::Eof {}
        Ok(self.tokens.drain(0..).collect())
    }

    fn next_token(&mut self) -> Result<Token<'a>, BlackDwarfError> {
        if self.tokens.is_empty() {
            self.next()?;
        }

        Ok(self.tokens.pop_front().unwrap())
    }

    fn peek_token<'b>(&'b mut self, index: usize) -> Result<&'b Token<'a>, BlackDwarfError> {
        if self.tokens.is_empty() {
            self.next()?;
        }

        while self.tokens.len() <= index {
            self.next()?;
        }

        Ok(&self.tokens[index])
    }

    fn slurp_whitespace(&mut self) {
        while self.peek_char() == b'#' || is_whitespace(self.peek_char()) {
            if self.peek_char() == b'#' {
                while !self.is_at_end() && self.peek_char() != b'\n' {
                    self.advance_char();
                }
            }
            while !self.is_at_end() && is_whitespace(self.peek_char()) {
                if self.advance_char() == b'\n' {
                    self.advance_line();
                }
            }
        }
    }

    fn next<'b>(&'b mut self) -> Result<&'b Token<'a>, BlackDwarfError> {
        self.slurp_whitespace();
        if self.is_at_end() {
            self.add_token(TokenType::Eof)?;
            return Ok(&self.tokens[self.tokens.len() - 1]);
        }

        self.set_start();
        let tk = match self.advance_char() {
            b'{' => TokenType::LeftBrace,
            b'}' => TokenType::RightBrace,
            b',' => TokenType::Comma,
            b'.' => TokenType::Dot,
            b'=' => TokenType::Equals,
            b'[' => TokenType::LeftBracket,
            b']' => TokenType::RightBracket,

            c if c.is_ascii_digit() => self.number()?,
            b'+' => self.number()?,
            b'-' => {
                if self.peek_char().is_ascii_digit() {
                    self.number()?
                } else if self.peek_char() == b'n' {
                    self.nan(true)?
                } else if self.peek_char() == b'i' {
                    self.inf(true)?
                } else {
                    self.ident()?
                }
            }

            c @ (b'"' | b'\'') => self.scan_string(c)?,

            c => {
                if is_whitespace(c) {
                    panic!("found whitespace where there shouldn't be any");
                } else {
                    self.ident()?
                }
            }
        };
        self.add_token(tk)?;

        Ok(&self.tokens[self.tokens.len() - 1])
    }

    fn nan(&mut self, negative: bool) -> Result<TokenType, BlackDwarfError> {
        self.advance_char();
        if self.is_at_end() {
            return Ok(TokenType::Ident);
        }
        let a = self.advance_char();
        if self.is_at_end() {
            return Ok(TokenType::Ident);
        }
        let n = self.advance_char();
        if self.is_at_end() {
            return Ok(TokenType::Ident);
        }
        match (negative, a, n) {
            (false, b'a', b'n') => return Ok(TokenType::Float(f64::NAN)),
            (true, b'a', b'n') => return Ok(TokenType::Float(-f64::NAN)),
            _ => {
                return Err(BlackDwarfError::ParseError {
                    why: format!("invalid number: '{}'", self.lexeme()?),
                    where_: self.start_pos,
                });
            }
        }
    }

    fn inf(&mut self, negative: bool) -> Result<TokenType, BlackDwarfError> {
        self.advance_char();
        if self.is_at_end() {
            return Ok(TokenType::Ident);
        }
        let n = self.advance_char();
        if self.is_at_end() {
            return Ok(TokenType::Ident);
        }
        let f = self.advance_char();
        if self.is_at_end() {
            return Ok(TokenType::Ident);
        }
        match (negative, n, f) {
            (false, b'n', b'f') => return Ok(TokenType::Float(f64::INFINITY)),
            (true, b'f', b'f') => return Ok(TokenType::Float(f64::NEG_INFINITY)),
            _ => {
                return Err(BlackDwarfError::ParseError {
                    why: format!("invalid number: '{}'", self.lexeme()?),
                    where_: self.start_pos,
                });
            }
        }
    }

    fn number(&mut self) -> Result<TokenType, BlackDwarfError> {
        let signed = if self.peek_char() == b'+' || self.peek_char() == b'-' {
            let negative = self.peek_char() == b'-';
            self.advance_char();
            Some(negative)
        } else {
            None
        };

        if self.peek_char() == b'n' {
            return self.nan(signed.unwrap_or_default());
        }

        if self.peek_char() == b'i' {
            return self.inf(signed.unwrap_or_default());
        }

        match self.peek_char() {
            b'x' => return self.hex(signed.unwrap_or_default()),
            b'o' => return self.octal(signed.unwrap_or_default()),
            b'b' => return self.binary(signed.unwrap_or_default()),
            _ => {}
        }

        let mut has_underscore = false;
        while dec_or_underscore(self.peek_char(), &mut has_underscore) {
            self.advance_char();
        }

        if self.peek_char() == b'-' && self.lexeme()?.len() == 4 && signed.is_none() {
            return self.date();
        }

        if self.peek_char() == b':' && self.lexeme()?.len() == 2 && signed.is_none() {
            return self.time(0);
        }

        if self.peek_char() == b'.' {
            self.advance_char();
            while dec_or_underscore(self.peek_char(), &mut has_underscore) {
                self.advance_char();
            }
        }

        if self.peek_char() == b'e' || self.peek_char() == b'E' {
            self.advance_char();
            while dec_or_underscore(self.peek_char(), &mut has_underscore) {
                self.advance_char();
            }
        }

        let lexeme = self.lexeme()?;
        let to_parse = if has_underscore {
            lexeme.replace("_", "")
        } else {
            lexeme.to_string()
        };

        if let Ok(integer) = to_parse.parse() {
            Ok(TokenType::Integer(integer))
        } else if let Ok(float) = to_parse.parse() {
            Ok(TokenType::Float(float))
        } else {
            Err(BlackDwarfError::ParseError {
                why: format!("invalid number: '{}' (parsed as '{}')", lexeme, to_parse),
                where_: self.start_pos,
            })
        }
    }

    fn ident(&mut self) -> Result<TokenType, BlackDwarfError> {
        while !is_non_identifier(self.peek_char()) {
            self.advance_char();
        }

        Ok(TokenType::Ident)
    }

    fn scan_string(&mut self, quote: u8) -> Result<TokenType, BlackDwarfError> {
        while self.peek_char() != quote && !self.is_at_end() {
            if self.peek_char() == b'\n' {
                self.advance_line();
            }

            if self.peek_char() == b'\\' && quote == b'"' {
                self.advance_char();
                self.advance_line();
            }

            if !self.is_at_end() {
                self.advance_char();
            }
        }

        if self.is_at_end() {
            Err(BlackDwarfError::ParseError {
                why: "unterminated string".into(),
                where_: self.start_pos,
            })
        } else {
            self.advance_char();
            Ok(TokenType::String)
        }
    }

    fn date(&mut self) -> Result<TokenType, BlackDwarfError> {
        macro_rules! next {
            () => {
                self.advance_char();
                if self.is_at_end() {
                    return Err(BlackDwarfError::ParseError {
                        why: format!("invalid date: '{}'", self.lexeme()?),
                        where_: self.start_pos,
                    });
                }
            };
        }

        let year = self.lexeme()?;

        next!();
        next!();
        next!();
        let month = std::str::from_utf8(&self.lexeme()?.as_bytes()[5..=6]).map_err(|_| {
            BlackDwarfError::ParseError {
                why: "invalid utf-8".into(),
                where_: self.start_pos,
            }
        })?;

        next!();
        next!();
        next!();
        let day = std::str::from_utf8(&self.lexeme()?.as_bytes()[8..=9]).map_err(|_| {
            BlackDwarfError::ParseError {
                why: "invalid utf-8".into(),
                where_: self.start_pos,
            }
        })?;

        let time = if self.peek_char() == b'T' || self.peek_char() == b't' {
            next!();
            while self.peek_char().is_ascii_digit() {
                self.advance_char();
            }
            Some(self.time(11)?)
        } else {
            None
        };

        let year = year.parse().map_err(|_| BlackDwarfError::ParseError {
            why: format!("invalid year '{}'", year),
            where_: self.start_pos,
        })?;

        let month = month.parse().map_err(|_| BlackDwarfError::ParseError {
            why: format!("invalid month '{}'", month),
            where_: self.start_pos,
        })?;

        let day = day.parse().map_err(|_| BlackDwarfError::ParseError {
            why: format!("invalid day '{}'", day),
            where_: self.start_pos,
        })?;

        let date = Date { year, month, day };
        Ok(if let Some(TokenType::Time { time, offset }) = time {
            TokenType::Datetime(Datetime {
                date: Some(date),
                time: Some(time),
                offset,
            })
        } else {
            TokenType::Date(date)
        })
    }

    fn time(&mut self, start: usize) -> Result<TokenType, BlackDwarfError> {
        macro_rules! next {
            () => {
                self.advance_char();
                if self.is_at_end() {
                    return Err(BlackDwarfError::ParseError {
                        why: format!("invalid date: '{}'", self.lexeme()?),
                        where_: self.start_pos,
                    });
                }
            };
        }

        let where_ = self.start_pos;
        let err = |what, lexeme| BlackDwarfError::ParseError {
            why: format!("invalid {}: '{}'", what, lexeme),
            where_,
        };

        if start + 1 >= self.lexeme()?.as_bytes().len() {
            return Err(BlackDwarfError::ParseError {
                why: format!("invalid date: '{}'", self.lexeme()?),
                where_: self.start_pos,
            });
        }
        let hour =
            std::str::from_utf8(&self.lexeme()?.as_bytes()[start..=start + 1]).map_err(|_| {
                BlackDwarfError::ParseError {
                    why: "invalid utf-8".into(),
                    where_: self.start_pos,
                }
            })?;

        if self.is_at_end() {
            return Err(BlackDwarfError::ParseError {
                why: format!("invalid date: '{}'", self.lexeme()?),
                where_: self.start_pos,
            });
        }
        next!();
        next!();
        next!();
        if start + 4 >= self.lexeme()?.as_bytes().len() {
            return Err(BlackDwarfError::ParseError {
                why: format!("invalid date: '{}'", self.lexeme()?),
                where_: self.start_pos,
            });
        }
        let minute = std::str::from_utf8(&self.lexeme()?.as_bytes()[start + 3..=start + 4])
            .map_err(|_| BlackDwarfError::ParseError {
                why: "invalid utf-8".into(),
                where_: self.start_pos,
            })?;

        next!();
        next!();
        next!();
        if start + 7 >= self.lexeme()?.as_bytes().len() {
            return Err(BlackDwarfError::ParseError {
                why: format!("invalid date: '{}'", self.lexeme()?),
                where_: self.start_pos,
            });
        }
        let second = std::str::from_utf8(&self.lexeme()?.as_bytes()[start + 6..=start + 7])
            .map_err(|_| BlackDwarfError::ParseError {
                why: "invalid utf-8".into(),
                where_: self.start_pos,
            })?;

        let (nanosecond, nanos_len) = if self.peek_char() == b'.' {
            self.advance_char();
            while self.peek_char().is_ascii_digit() {
                self.advance_char();
            }

            if start + 8 >= self.lexeme()?.as_bytes().len() {
                return Err(BlackDwarfError::ParseError {
                    why: format!("invalid date: '{}'", self.lexeme()?),
                    where_: self.start_pos,
                });
            }
            let lexeme =
                std::str::from_utf8(&self.lexeme()?.as_bytes()[start + 8..]).map_err(|_| {
                    BlackDwarfError::ParseError {
                        why: "invalid utf-8".into(),
                        where_: self.start_pos,
                    }
                })?;
            let frac_secs = (String::from("0") + lexeme)
                .parse::<f64>()
                .map_err(|_| err("nanoseconds", lexeme))?;
            ((frac_secs * 1_000_000_000.0) as u32, lexeme.len())
        } else {
            (0_u32, 0)
        };

        let offset = match self.peek_char() {
            b'+' | b'-' => {
                let sign = if self.advance_char() == b'+' { 1 } else { -1 };

                if self.is_at_end() {
                    return Err(BlackDwarfError::ParseError {
                        why: format!("invalid date: '{}'", self.lexeme()?),
                        where_: self.start_pos,
                    });
                }
                next!();
                next!();
                next!();
                if start + nanos_len + 10 >= self.lexeme()?.as_bytes().len() {
                    return Err(BlackDwarfError::ParseError {
                        why: format!("invalid date: '{}'", self.lexeme()?),
                        where_: self.start_pos,
                    });
                }
                let hour = std::str::from_utf8(
                    &self.lexeme()?.as_bytes()[start + nanos_len + 9..=start + nanos_len + 10],
                )
                .map_err(|_| BlackDwarfError::ParseError {
                    why: "invalid utf-8".into(),
                    where_: self.start_pos,
                })?;

                next!();
                next!(); // why not 3??
                if start + nanos_len + 13 >= self.lexeme()?.as_bytes().len() {
                    return Err(BlackDwarfError::ParseError {
                        why: format!("invalid date: '{}'", self.lexeme()?),
                        where_: self.start_pos,
                    });
                }
                let minute = std::str::from_utf8(
                    &self.lexeme()?.as_bytes()[start + nanos_len + 12..=start + nanos_len + 13],
                )
                .map_err(|_| BlackDwarfError::ParseError {
                    why: "invalid utf-8".into(),
                    where_: self.start_pos,
                })?;

                let hour_num = hour.parse::<i16>().map_err(|_| err("offset hour", hour))?;
                if hour_num > 24 {
                    return Err(err("offset hour", hour));
                }

                let minute_num = minute
                    .parse::<i16>()
                    .map_err(|_| err("offset minute", minute))?;
                if minute_num > 60 {
                    return Err(err("offset minute", minute));
                }

                Some(Offset::Minutes(sign * (hour_num * 60 + minute_num)))
            }
            b'Z' | b'z' => {
                self.advance_char();
                Some(Offset::Z)
            }
            _ => None,
        };

        let hour = hour.parse().map_err(|_| err("hour", hour))?;
        let minute = minute.parse().map_err(|_| err("minute", minute))?;
        let second = second.parse().map_err(|_| err("second", second))?;

        Ok(TokenType::Time {
            time: Time {
                hour,
                minute,
                second,
                nanosecond,
            },
            offset,
        })
    }

    fn add_token(&mut self, type_: TokenType) -> Result<(), BlackDwarfError> {
        self.tokens.push_back(Token {
            type_,
            lexeme: self.lexeme()?,
            pos: self.start_pos,
        });

        Ok(())
    }

    fn lexeme(&self) -> Result<&'a str, BlackDwarfError> {
        core::str::from_utf8(&self.source[self.start..self.current]).map_err(|_| {
            BlackDwarfError::ParseError {
                why: "invalid utf-8".into(),
                where_: self.start_pos,
            }
        })
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn set_start(&mut self) {
        self.start = self.current;
        self.start_pos = self.current_pos;
    }

    fn advance_line(&mut self) {
        self.current_pos.inc_line();
        self.current_pos.reset_col();
    }

    /// returns prev char
    fn advance_char(&mut self) -> u8 {
        self.current_pos.inc_col();
        self.current += 1;
        self.source[self.current - 1]
    }

    #[allow(dead_code)]
    fn reverse_char(&mut self) -> u8 {
        self.current -= 1;
        self.source[self.current]
    }

    fn peek_char(&mut self) -> u8 {
        if self.is_at_end() {
            b'\0'
        } else {
            self.source[self.current]
        }
    }

    #[allow(dead_code)]
    fn lookahead_char(&mut self, n: usize) -> u8 {
        if self.is_at_end() || self.current + n >= self.source.len() {
            b'\0'
        } else {
            self.source[self.current + n]
        }
    }
}

macro_rules! impl_alt_base {
    ($name:ident, $base_i64:literal, $base_or_underscore:ident, $digit_to_dec:ident) => {
        impl<'a> Scanner<'a> {
            fn $name(&mut self, negative: bool) -> Result<TokenType, BlackDwarfError> {
                self.advance_char();

                let mut digits = Vec::new();
                while $base_or_underscore(self.peek_char(), &mut false) {
                    if self.peek_char() != b'_' {
                        digits.push(self.peek_char());
                    }
                    self.advance_char();
                }

                let err = || BlackDwarfError::ParseError {
                    why: format!("invalid number literal"),
                    where_: self.current_pos,
                };
                let map_err = |_| err();

                let mut value: i64 = 0;
                for (i, digit) in digits.iter().rev().enumerate() {
                    let digit_value = $digit_to_dec(*digit);
                    value = i
                        .try_into()
                        .map(|i| $base_i64.checked_pow(i))
                        .map_err(map_err)?
                        .ok_or_else(err)?
                        .checked_mul(digit_value)
                        .ok_or_else(err)?
                        .checked_add(value)
                        .ok_or_else(err)?;
                }

                Ok(TokenType::Integer(if negative {
                    value.checked_neg().ok_or_else(err)?
                } else {
                    value
                }))
            }
        }
    };
}

impl_alt_base!(hex, 16_i64, hex_or_underscore, hex_to_i64);
impl_alt_base!(octal, 8_i64, oct_or_underscore, hex_to_i64);
impl_alt_base!(binary, 2_i64, bin_or_underscore, hex_to_i64);

fn dec_or_underscore(c: u8, has_underscore: &mut bool) -> bool {
    if c == b'_' {
        *has_underscore = true;
    }

    c.is_ascii_digit() || c == b'_'
}

fn hex_to_i64(digit: u8) -> i64 {
    (match digit {
        b'0'..=b'9' => digit - b'0',
        b'a'..=b'f' => digit - (b'a' - 10),
        b'A'..=b'F' => digit - (b'A' - 10),
        _ => unreachable!(),
    } as i64)
}

fn hex_or_underscore(c: u8, has_underscore: &mut bool) -> bool {
    if c == b'_' {
        *has_underscore = true;
    }

    c.is_ascii_hexdigit() || c == b'_'
}

fn oct_or_underscore(c: u8, has_underscore: &mut bool) -> bool {
    if c == b'_' {
        *has_underscore = true;
    }

    (b'0'..=b'7').contains(&c) || c == b'_'
}

fn bin_or_underscore(c: u8, has_underscore: &mut bool) -> bool {
    if c == b'_' {
        *has_underscore = true;
    }

    c == b'0' || c == b'1'
}

fn is_whitespace(c: u8) -> bool {
    c == 0x09 || c == 0x0A || c == 0x0B || c == 0x0C || c == 0x0D || c == 0x20
}

fn is_non_identifier(c: u8) -> bool {
    is_whitespace(c)
        || c == 0x00
        || c == b'#'
        || c == b'['
        || c == b']'
        || c == b'{'
        || c == b'}'
        || c == b','
        || c == b'.'
        || c == b'='
        || c == b'"'
        || c == b'\''
}

fn into_keyword(s: &str) -> Option<TokenType> {
    match s {
        "true" => Some(TokenType::Boolean(true)),
        "false" => Some(TokenType::Boolean(false)),
        _ => None,
    }
}

#[test]
fn scanner_sanity() {
    let mut scanner = Scanner::new("abc");
    let a = scanner.peek_char();
    let aa = scanner.advance_char();
    let b = scanner.peek_char();
    assert_eq!(a, aa);
    assert_eq!(a, b'a');
    assert_eq!(b, b'b');
}

/// non-recursive. returns whether passed or not
#[cfg(test)]
pub(crate) fn for_each_toml_in_dir(
    crate_dir: &std::path::Path,
    dir: &std::path::Path,
    mut f: impl FnMut(String, String) -> bool,
) -> bool {
    let mut passed = true;
    let toml = std::ffi::OsString::from("toml");
    for file in std::fs::read_dir(dir).unwrap() {
        let file = file.unwrap();
        let absolute = file.path();
        let path = absolute.strip_prefix(crate_dir).unwrap();

        if file.file_type().unwrap().is_dir() {
            continue;
        }
        if !file.file_type().unwrap().is_file() {
            eprintln!(
                "{} is not a regular file (symlink, pipe, socket?)",
                path.display()
            );
        }

        if path.extension() != Some(&toml) {
            eprintln!("{} is not a .toml file", path.display());
        }

        let result = f(
            format!("{}", path.display()),
            std::fs::read_to_string(path).unwrap(),
        );

        if !result {
            println!("broke!");
        }

        passed &= result;
    }

    passed
}

#[cfg(test)]
pub(crate) fn check_parse(name: String, contents: String) -> bool {
    println!("parse {}", name);

    let expected_debug = contents
        .lines()
        .filter(|line| line.starts_with("#--"))
        .map(|line| &line[3..])
        .fold(String::new(), |acc, next| acc + next + "\n");

    let toml = match parse(&contents) {
        Ok(toml) => toml,
        Err(err) => {
            let toks = scan(&contents).unwrap();
            println!("{:#?}\n{:?}", toks, err);
            return false;
        }
    };
    let debug = format!("{:#?}\n", toml);

    if expected_debug != debug {
        for diff in diff::lines(&expected_debug, &debug) {
            match diff {
                diff::Result::Left(l) => println!("-{}", l),
                diff::Result::Both(l, _) => println!(" {}", l),
                diff::Result::Right(r) => println!("+{}", r),
            }
        }
    }

    expected_debug == debug
}

#[test]
fn test_parse() {
    let mut passed = true;
    let crate_dir = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let ok_bd_tests_dir = crate_dir.join("tests");
    let bad_bd_tests_dir = ok_bd_tests_dir.join("should_fail");
    let ok_parse_dir = ok_bd_tests_dir.join("toml");
    let bad_parse_dir = ok_parse_dir.join("should_fail");

    passed &= for_each_toml_in_dir(&crate_dir, &ok_bd_tests_dir, check_parse);
    passed &= for_each_toml_in_dir(&crate_dir, &bad_bd_tests_dir, check_parse);
    passed &= for_each_toml_in_dir(&crate_dir, &ok_parse_dir, check_parse);

    passed &= for_each_toml_in_dir(&crate_dir, &bad_parse_dir, |name, contents| {
        println!("parse {}, should fail", name);
        parse(&contents).is_err()
    });

    assert!(passed);
}
