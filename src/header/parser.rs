use nom::{
  branch::alt,
  bytes::complete::tag,
  character::complete::{char, digit0, digit1, multispace0, one_of},
  combinator::{map, opt, recognize},
  sequence::{delimited, pair, preceded},
  IResult, InputLength, InputTake, ParseTo, Parser,
};

type Slice<'a> = &'a [u8];
type Result<'a, O = Slice<'a>> = IResult<Slice<'a>, O>;

//
// Logical
//
pub fn logical(s: Slice) -> Result<bool> {
  map(one_of("TF"), |c| c == 'T')(s)
}

//
// Integer
//
pub fn integer(i: Slice) -> Result<i64> {
  let (i, s) = recognize_integer(i)?;
  match s.parse_to() {
    Some(v) => Ok((i, v)),
    None => Err(nom::Err::Error(nom::error::Error::new(
      i,
      nom::error::ErrorKind::Digit,
    ))),
  }
}

fn sign(i: Slice) -> Result<bool> {
  let (i, s) = opt(alt((char('+'), char('-'))))(i)?;
  Ok((i, s.unwrap_or('+') == '+'))
}

fn recognize_integer(s: Slice) -> Result {
  recognize(pair(sign, digit1))(s)
}

//
// Real
//
pub fn real(i: Slice) -> Result<f64> {
  let (i, a) = recognize(real_decimal)(i)?;
  let (i, exp) = opt(one_of("eEdD"))(i)?;
  match exp {
    Some(_) => {
      let (i, b) = recognize(integer)(i)?;
      let s = unsafe { std::str::from_utf8_unchecked(a) }.to_owned()
        + "e"
        + unsafe { std::str::from_utf8_unchecked(b) };
      parse_float_from_str(i, &s)
    }
    None => {
      let s = unsafe { std::str::from_utf8_unchecked(a) }.to_owned();
      parse_float_from_str(i, &s)
    }
  }
}

fn real_decimal(i: Slice) -> Result<(bool, Slice, Slice)> {
  let (i, (sign, pre)) = pair(sign, digit0)(i)?;
  let (i, _) = opt(char('.'))(i)?;
  let (i, post) = digit0(i)?;
  Ok((i, (sign, pre, post)))
}

fn parse_float_from_str<'a>(i: &'a [u8], s: &str) -> Result<'a, f64> {
  match s.parse() {
    Ok(v) => Ok((i, v)),
    Err(_) => Err(nom::Err::Error(nom::error::Error::new(
      i,
      nom::error::ErrorKind::Digit,
    ))),
  }
}

//
// Complex
//
pub fn complex_integer(i: Slice) -> Result<(i64, i64)> {
  complex(integer, i)
}

pub fn complex_real(i: Slice) -> Result<(f64, f64)> {
  complex(real, i)
}

pub fn complex<'a, T, F>(f: F, i: Slice<'a>) -> Result<(T, T)>
where
  F: Parser<Slice<'a>, T, nom::error::Error<Slice<'a>>> + Copy,
{
  delimited(
    char('('),
    pair(
      delimited(multispace0, f, multispace0),
      preceded(char(','), delimited(multispace0, f, multispace0)),
    ),
    char(')'),
  )(i)
}

//
// Character string values
//
pub fn character_string(s: Slice) -> Result {
  delimited(tag("'"), escaped_string, tag("'"))(s)
}

fn escaped_string(s: Slice) -> Result {
  match s
    .iter()
    .scan((false, 0u8), |(esc, prev), &item| {
      let flag = !*esc && item != b'\'' && *prev == b'\'';
      *esc = !*esc && item == b'\'' && *prev == b'\'';
      *prev = item;
      Some(flag)
    })
    .position(|item| item)
  {
    Some(i) => Ok(s.take_split(i - 1)),
    None => {
      let i = if s.last() == Some(&b'\'') {
        s.input_len() - 1
      } else {
        s.input_len()
      };
      Ok(s.take_split(i))
    }
  }
}

//
// Comment
//
pub fn comment(i: Slice) -> Result {
  let (i, _) = char('/')(i)?;
  let (i, _) = multispace0(i)?;
  Ok((&i[i.len()..], i))
}

#[cfg(test)]
mod tests {
  use super::*;
  use rstest::*;

  #[test]
  fn test_logical() {
    let result = logical(b"T").unwrap();
    assert_eq!(result, (&b""[..], true));

    let result = logical(b"F").unwrap();
    assert_eq!(result, (&b""[..], false));
  }

  #[rstest]
  #[case(b"12345", 12345i64)]
  #[case(b"+12345", 12345i64)]
  #[case(b"0012345", 12345i64)]
  #[case(b"+0012345", 12345i64)]
  #[case(b"-12345", -12345i64)]
  #[case(b"-0012345", -12345i64)]
  fn test_integer(#[case] data: &[u8], #[case] expected: i64) {
    let result = integer(data).unwrap();
    assert_eq!(result, (&b""[..], expected));
  }

  #[rstest]
  #[case(b"1", 1.0f64)]
  #[case(b"1.23", 1.23f64)]
  #[case(b"+1.23", 1.23f64)]
  #[case(b"-1.23", -1.23f64)]
  #[case(b"-01.23", -1.23f64)]
  #[case(b"1.23e2", 1.23e2f64)]
  #[case(b"1.23E2", 1.23e2f64)]
  #[case(b"1.23d2", 1.23e2f64)]
  #[case(b"1.23D2", 1.23e2f64)]
  #[case(b"1.23e02", 1.23e2f64)]
  #[case(b"1.23e-2", 1.23e-2f64)]
  fn test_real(#[case] data: &[u8], #[case] expected: f64) {
    let result = real(data).unwrap();
    assert_eq!(result, (&b""[..], expected));
  }

  #[rstest]
  #[case(b"(1,2)", (1, 2))]
  #[case(b"(1, +2)", (1, 2))]
  #[case(b"( -1, 2 )", (-1, 2))]
  fn test_complex_integer(#[case] data: &[u8], #[case] expected: (i64, i64)) {
    let result = complex_integer(data).unwrap();
    assert_eq!(result, (&b""[..], expected));
  }

  #[rstest]
  #[case(b"(1,2)", (1.0, 2.0))]
  #[case(b"(1.12e2, +2)", (1.12e2, 2.0))]
  #[case(b"( -1, 2.34e1 )", (-1.0, 2.34e1))]
  fn test_complex_real(#[case] data: &[u8], #[case] expected: (f64, f64)) {
    let result = complex_real(data).unwrap();
    assert_eq!(result, (&b""[..], expected));
  }

  #[rstest]
  #[case(b"'hello world'", b"hello world")]
  #[case(b"'hello '' world'", b"hello '' world")]
  #[case(b"'hello '''", b"hello ''")]
  #[case(b"'hello '''''", b"hello ''''")]
  #[case(b"'''hello '", b"''hello ")]
  fn test_character_string(#[case] data: &[u8], #[case] expected: &[u8]) {
    let result = character_string(data).unwrap();
    assert_eq!(result, (&b""[..], expected));
  }

  #[rstest]
  #[case(b"/comment", b"comment")]
  #[case(b"/ comment", b"comment")]
  #[case(b"/ ", b"")]
  fn test_comment(#[case] data: &[u8], #[case] expected: &[u8]) {
    let result = comment(data).unwrap();
    assert_eq!(result, (&b""[..], expected));
  }
}
