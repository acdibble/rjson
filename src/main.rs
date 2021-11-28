use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::iter::Peekable;
use std::str::CharIndices;

#[derive(Debug, PartialEq)]
enum Value {
    Null,
    True,
    False,
    String(String),
    Number(f64),
    Array(Vec<Value>),
    Object(Vec<(String, Value)>),
}

impl Value {
    fn to_string(self) -> Result<String, ()> {
        match self {
            Value::String(string) => Ok(string),
            _ => Err(()),
        }
    }
}

type ParseResult = std::result::Result<Value, String>;

struct Parser<'a> {
    chars: Peekable<CharIndices<'a>>,
}

impl<'a> Parser<'a> {
    fn error(&self, data: Option<(usize, char)>) -> ParseResult {
        match data {
            Some((index, ch)) => Err(format!("Unexpected token '{}' at position '{}'", ch, index)),
            _ => Err("Unexpected end of input".to_owned()),
        }
    }

    fn consume_whitespace(&mut self) {
        while let Some((_, ' ' | '\t' | '\r' | '\n')) = self.chars.peek() {
            self.chars.next();
        }
    }

    fn consume(&mut self, ch: char) -> bool {
        match self.chars.peek() {
            Some(&(_, next)) if next == ch => {
                self.chars.next();
                true
            }
            _ => false,
        }
    }

    fn try_consume(&mut self, ch: char) -> std::result::Result<(), String> {
        match self.chars.next() {
            Some((_, next)) if ch == next => (),
            option => {
                self.error(option)?;
            }
        };

        Ok(())
    }

    fn parse(&mut self) -> ParseResult {
        let value = self.parse_value()?;

        match self.chars.peek() {
            None => Ok(value),
            Some(&data) => self.error(Some(data)),
        }
    }

    fn parse_value(&mut self) -> ParseResult {
        self.consume_whitespace();

        let result = match self.chars.next() {
            Some((_, 'n')) => self.parse_literal(&['u', 'l', 'l'], Value::Null),
            Some((_, 't')) => self.parse_literal(&['r', 'u', 'e'], Value::True),
            Some((_, 'f')) => self.parse_literal(&['a', 'l', 's', 'e'], Value::False),
            Some((_, '"')) => self.parse_string(),
            Some((_, '[')) => self.parse_array(),
            Some((_, '{')) => self.parse_object(),
            Some((_, value @ ('-' | '0'..='9'))) => self.parse_number(value),
            option => self.error(option),
        };

        self.consume_whitespace();

        result
    }

    fn parse_object(&mut self) -> ParseResult {
        self.consume_whitespace();

        let mut key_values = Vec::new();

        if !self.consume('}') {
            loop {
                self.consume_whitespace();

                self.try_consume('"')?;

                let key = self.parse_string()?;
                self.consume_whitespace();

                self.try_consume(':')?;

                let value = self.parse_value()?;
                key_values.push((key.to_string().unwrap(), value));

                if self.consume(',') {
                    continue;
                }

                self.try_consume('}')?;
                break;
            }
        }

        Ok(Value::Object(key_values))
    }

    fn parse_array(&mut self) -> ParseResult {
        let mut array = Vec::new();
        self.consume_whitespace();

        if !self.consume(']') {
            loop {
                let value = self.parse_value()?;
                array.push(value);

                if self.consume(',') {
                    continue;
                }

                self.try_consume(']')?;
                break;
            }
        }

        Ok(Value::Array(array))
    }

    fn collect_digits(&mut self, buffer: &mut String) {
        while let Some(&(_, ch)) = self.chars.peek() {
            match ch {
                '0'..='9' => {
                    self.chars.next();
                    buffer.push(ch);
                }
                _ => break,
            }
        }
    }

    fn parse_number(&mut self, ch: char) -> ParseResult {
        let mut num = String::new();
        match ch {
            '0' => match self.chars.peek() {
                Some((_, '.' | 'E' | 'e')) => (),
                _ => return Ok(Value::Number(0f64)),
            },
            '-' => match self.chars.next() {
                Some((_, next @ '0'..='9')) => {
                    num.push(ch);
                    num.push(next);
                    self.collect_digits(&mut num);
                }
                option => return self.error(option),
            },
            '1'..='9' => {
                num.push(ch);
                self.collect_digits(&mut num);
            }
            _ => unreachable!(),
        }

        if self.consume('.') {
            num.push('.');
            self.collect_digits(&mut num);
        }

        let mut pow = String::new();
        if matches!(self.chars.peek(), Some((_, 'e' | 'E'))) {
            self.chars.next();

            match self.chars.peek() {
                Some(&(_, ch @ ('+' | '-'))) => {
                    self.chars.next();
                    pow.push(ch)
                }
                _ => (),
            }

            match self.chars.next() {
                Some((_, ch @ '0'..='9')) => pow.push(ch),
                option => return self.error(option),
            }

            self.collect_digits(&mut pow);
        }

        match num.parse::<f64>() {
            Ok(value) => Ok(Value::Number(value)),
            _ => Err("Failed to parse number".to_owned()),
        }
    }

    fn parse_unicode(&mut self, start: usize) -> std::result::Result<char, String> {
        let mut value = 0u32;

        for i in (0..4).rev() {
            value += 16u32.pow(i)
                * match self.chars.next() {
                    Some((_, '0')) => 0,
                    Some((_, '1')) => 1,
                    Some((_, '2')) => 2,
                    Some((_, '3')) => 3,
                    Some((_, '4')) => 4,
                    Some((_, '5')) => 5,
                    Some((_, '6')) => 6,
                    Some((_, '7')) => 7,
                    Some((_, '8')) => 8,
                    Some((_, '9')) => 9,
                    Some((_, 'a' | 'A')) => 10,
                    Some((_, 'b' | 'B')) => 11,
                    Some((_, 'c' | 'C')) => 12,
                    Some((_, 'd' | 'D')) => 13,
                    Some((_, 'e' | 'E')) => 14,
                    Some((_, 'f' | 'F')) => 15,
                    option => {
                        self.error(option)?;
                        unreachable!()
                    }
                }
        }

        match char::from_u32(value) {
            Some(ch) => Ok(ch),
            None => {
                self.error(Some((start, 'u')))?;
                unreachable!()
            }
        }
    }

    fn parse_string(&mut self) -> ParseResult {
        let mut string = String::new();
        while let Some((_, ch)) = self.chars.next() {
            match ch {
                '"' => return Ok(Value::String(string)),
                '\\' => match self.chars.next() {
                    Some((_, ch @ ('"' | '\\' | '/'))) => string.push(ch),
                    Some((_, 'n')) => string.push('\n'),
                    // Some((_, 'b')) => string.push('\b'),
                    Some((_, 'r')) => string.push('\r'),
                    // Some((_, 'f')) => string.push('\f'),
                    Some((_, 't')) => string.push('\t'),
                    Some((start, 'u')) => string.push(self.parse_unicode(start)?),
                    data => return self.error(data),
                },
                _ => string.push(ch),
            }
        }

        self.error(None)
    }

    fn parse_literal(&mut self, values: &[char], value: Value) -> ParseResult {
        for &expected in values {
            match self.chars.next() {
                Some((_, ch)) if ch == expected => (),
                option => return self.error(option),
            }
        }

        Ok(value)
    }
}

fn parse(string: &str) -> ParseResult {
    let mut parser = Parser {
        chars: string.char_indices().peekable(),
    };

    parser.parse()
}

mod test {
    use crate::{parse, Value};

    #[test]
    fn test_parse() {
        assert_eq!(parse("null"), Ok(Value::Null));
        assert_eq!(parse("true"), Ok(Value::True));
        assert_eq!(parse("false"), Ok(Value::False));
        assert_eq!(parse("   null   "), Ok(Value::Null));
        assert_eq!(parse("\"\""), Ok(Value::String("".to_owned())));
        assert_eq!(parse("\"\\u0041\""), Ok(Value::String("A".to_owned())));
        assert_eq!(parse("\"null\""), Ok(Value::String("null".to_owned())));
        assert_eq!(
            parse("\"nu\\\"ll\""),
            Ok(Value::String("nu\"ll".to_owned()))
        );
        assert_eq!(
            parse("\"nu\\\\ll\""),
            Ok(Value::String("nu\\ll".to_owned()))
        );
        assert_eq!(
            parse("\"nu\\\\ll\"1"),
            Err("Unexpected token '1' at position '8'".to_owned())
        );
        assert_eq!(
            parse("\"nu\\\\ll\"  1"),
            Err("Unexpected token '1' at position '10'".to_owned())
        );
        assert_eq!(
            parse("\"nu\\\\ll"),
            Err("Unexpected end of input".to_owned())
        );
        assert_eq!(parse("0"), Ok(Value::Number(0f64)));
        assert_eq!(parse("-0"), Ok(Value::Number(-0f64)));
        assert_eq!(parse("1"), Ok(Value::Number(1f64)));
        assert_eq!(parse("1.0"), Ok(Value::Number(1.0)));
        assert_eq!(parse("3.14"), Ok(Value::Number(3.14)));
        assert_eq!(parse("[]"), Ok(Value::Array(Vec::new())));
        assert_eq!(parse("["), Err("Unexpected end of input".to_owned()));
        assert_eq!(parse("   [     ]   "), Ok(Value::Array(Vec::new())));
        assert_eq!(
            parse("[1]"),
            Ok(Value::Array(Vec::from([Value::Number(1.)])))
        );
        assert_eq!(
            parse("  [  1  ]  "),
            Ok(Value::Array(Vec::from([Value::Number(1.)])))
        );
        assert_eq!(
            parse("[1,]"),
            Err("Unexpected token ']' at position '3'".to_owned())
        );
        assert_eq!(
            parse("[,]"),
            Err("Unexpected token ',' at position '1'".to_owned())
        );
        assert_eq!(
            parse("[[[]]]"),
            Ok(Value::Array(Vec::from([Value::Array(Vec::from([
                Value::Array(Vec::new())
            ]))])))
        );
        assert_eq!(
            parse("[[[1], 2], 3]"),
            Ok(Value::Array(Vec::from([
                Value::Array(Vec::from([
                    Value::Array(Vec::from([Value::Number(1.)])),
                    Value::Number(2.),
                ])),
                Value::Number(3.)
            ])))
        );
        assert_eq!(parse("{}"), Ok(Value::Object(Vec::new())));
        assert_eq!(
            parse("{ \"hello\" : \"world\"  }  "),
            Ok(Value::Object(Vec::from([(
                "hello".to_owned(),
                Value::String("world".to_owned())
            )])))
        );
        assert_eq!(
            parse("{ \"c\" : \"u\"  ,  \"l\": 8  }  "),
            Ok(Value::Object(Vec::from([
                ("c".to_owned(), Value::String("u".to_owned())),
                ("l".to_owned(), Value::Number(8.))
            ])))
        );
        assert_eq!(
            parse("{\"c\":true,\"l\":null}"),
            Ok(Value::Object(Vec::from([
                ("c".to_owned(), Value::True),
                ("l".to_owned(), Value::Null)
            ])))
        );
        assert_eq!(
            parse("{\"c\":true,}"),
            Err("Unexpected token '}' at position '10'".to_owned())
        );
        assert_eq!(
            parse("{\"c\":tra}"),
            Err("Unexpected token 'a' at position '7'".to_owned())
        );
    }
}

fn main() {
    let args: Vec<_> = env::args().collect();

    match args.get(1) {
        None => std::process::exit(1),
        Some(string) => {
            let mut file = File::open(string).unwrap();
            let mut buffer = String::new();
            file.read_to_string(&mut buffer).unwrap();
            let start = std::time::Instant::now();
            parse(&buffer).unwrap();
            println!("{}", start.elapsed().as_secs_f64())
        }
    }
}
