use std::io::Write;

use rand::Rng;

mod value_set;
use value_set::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Symbol {
    Digit(u8),
    Plus,
    Minus,
    Times,
    Slash,
    Equals,
    Unknown,
}

impl std::fmt::Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Symbol::Digit(d) => {
                let idx = *d as usize;
                &"0123456789"[idx..=idx]
            }
            Symbol::Plus => "+",
            Symbol::Minus => "-",
            Symbol::Times => "*",
            Symbol::Slash => "/",
            Symbol::Equals => "=",
            Symbol::Unknown => "?",
        };

        write!(f, "{s}")
    }
}

impl std::str::FromStr for Symbol {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.as_bytes() {
            &[d @ b'0'..=b'9'] => Ok(Symbol::Digit((d - b'0') as _)),
            &[b'+'] => Ok(Symbol::Plus),
            &[b'-'] => Ok(Symbol::Minus),
            &[b'*'] => Ok(Symbol::Times),
            &[b'/'] => Ok(Symbol::Slash),
            &[b'='] => Ok(Symbol::Equals),
            &[b'?'] => Ok(Symbol::Unknown),
            _ => Err(()),
        }
    }
}

fn eval_digits(syms: &[Symbol]) -> Option<(i32, &[Symbol])> {
    let (neg, mut syms) = match syms {
        &[Symbol::Minus, ref tail @ ..] => (true, tail),
        slice => (false, slice),
    };

    let mut acc: i32 = 0;
    while let &[Symbol::Digit(digit), ref tail @ ..] = syms {
        acc = acc.checked_mul(10)?.checked_add(digit as _)?;
        syms = tail;
    }

    if neg {
        acc = -acc;
    }

    Some((acc, syms))
}

macro_rules! impl_eval_op {
    ($name:ident, $checked_op:ident, $symbol:ident, $recurse:ident) => {
        fn $name(syms: &[Symbol]) -> Option<(i32, &[Symbol])> {
            let (mut acc, mut syms) = $recurse(syms)?;
            while let [Symbol::$symbol, ref tail @ ..] = syms {
                let (val, tail) = $recurse(tail)?;
                acc = acc.$checked_op(val)?;
                syms = tail;
            }

            Some((acc, syms))
        }
    };
}

impl_eval_op!(eval_div, checked_div, Slash, eval_digits);
impl_eval_op!(eval_mul, checked_mul, Times, eval_div);
impl_eval_op!(eval_sub, checked_sub, Minus, eval_mul);
impl_eval_op!(eval_add, checked_add, Plus, eval_sub);

fn eval(syms: &[Symbol]) -> Option<i32> {
    let (val, tail) = eval_add(syms)?;
    if !tail.is_empty() {
        return None;
    }

    Some(val)
}

fn valid(syms: &[Symbol]) -> Option<bool> {
    let (left, syms) = eval_add(syms)?;
    let syms = match syms {
        [Symbol::Equals, tail @ ..] => tail,
        _ => return None,
    };

    let right = eval(syms)?;
    Some(left == right)
}

fn eval_ranged_digits_unsigned(mut syms: &[Symbol]) -> Option<(ValueSet, &[Symbol])> {
    if !matches!(syms, &[Symbol::Digit(_) | Symbol::Unknown, ..]) {
        return None;
    }

    let mut min_acc: i32 = 0;
    let mut max_acc: i32 = 0;

    let mut first_digit = true;
    while let [digit, tail @ ..] = syms {
        let (min_digit, max_digit) = match *digit {
            Symbol::Digit(d) => (d, d),
            Symbol::Unknown if first_digit => (1, 9),
            Symbol::Unknown => (0, 9),
            _ => break,
        };

        min_acc = min_acc * 10 + (min_digit as i32);
        max_acc = max_acc * 10 + (max_digit as i32);

        syms = tail;
        first_digit = false;
    }

    let set = ValueSet::from_range(min_acc..max_acc);
    Some((set, syms))
}

fn eval_ranged_digits(syms: &[Symbol]) -> Option<(ValueSet, &[Symbol])> {
    match syms {
        slice @ [Symbol::Digit(_), ..] => eval_ranged_digits_unsigned(slice),
        [Symbol::Minus, slice @ ..] => {
            let (range, residual) = eval_ranged_digits_unsigned(slice)?;
            Some((-range, residual))
        }
        full @ [Symbol::Unknown, tail @ ..] => {
            let pos_result = eval_ranged_digits_unsigned(full)?;
            let neg_range = match eval_ranged_digits_unsigned(tail) {
                Some((range, _)) => -range,
                None => return Some(pos_result),
            };

            let (pos_range, residual) = pos_result;
            Some((pos_range.union(&neg_range), residual))
        }
        _ => None,
    }
}

macro_rules! impl_eval_ranged_op {
    ($name:ident, $op:tt, $symbol:ident, $recurse:ident) => {
        fn $name(syms: &[Symbol]) -> Option<(ValueSet, &[Symbol])> {
            let (mut acc, mut syms) = $recurse(syms)?;
            while let [Symbol::$symbol, ref tail @ ..] = syms {
                let (val, tail) = $recurse(tail)?;
                acc = acc $op val;
                if acc.is_empty() {
                    return None;
                }
                syms = tail;
            }

            Some((acc, syms))
        }
    };
}

impl_eval_ranged_op!(eval_ranged_div, /, Slash, eval_ranged_digits);
impl_eval_ranged_op!(eval_ranged_mul, *, Times, eval_ranged_div);
impl_eval_ranged_op!(eval_ranged_sub, -, Minus, eval_ranged_mul);
impl_eval_ranged_op!(eval_ranged_add, +, Plus,  eval_ranged_sub);

//fn eval_ranged_div(syms: &[Symbol]) -> Option<(ValueSet, &[Symbol])> {
//    let (mut acc, mut syms) = eval_ranged_digits(syms)?;
//    while let [Symbol::Slash, ref tail @ ..] = syms {
//        let (val, tail) = eval_ranged_digits(tail)?;
//
//        let corners = [
//            acc.start.checked_div(val.start),
//            acc.end.checked_div(val.start),
//            acc.start.checked_div(val.end),
//            acc.end.checked_div(val.end),
//            val.contains(&1).then(|| acc.start),
//            val.contains(&1).then(|| acc.end),
//            val.contains(&-1).then(|| -acc.start),
//            val.contains(&-1).then(|| -acc.end),
//        ];
//
//        let min = *corners.iter().flatten().min()?;
//        let max = *corners.iter().flatten().max()?;
//        acc = min..max;
//
//        syms = tail;
//    }
//
//    Some((acc, syms))
//}
//
//fn eval_ranged_mul(syms: &[Symbol]) -> Option<(ValueSet, &[Symbol])> {
//    let (mut acc, mut syms) = eval_ranged_div(syms)?;
//    while let [Symbol::Times, ref tail @ ..] = syms {
//        let (val, tail) = eval_ranged_div(tail)?;
//
//        let corners = [
//            acc.start * val.start,
//            acc.end * val.start,
//            acc.start * val.end,
//            acc.end * val.end,
//        ];
//
//        let min = *corners.iter().min().unwrap();
//        let max = *corners.iter().max().unwrap();
//        acc = min..max;
//
//        syms = tail;
//    }
//
//    Some((acc, syms))
//}
//
//fn eval_ranged_sub(syms: &[Symbol]) -> Option<(ValueSet, &[Symbol])> {
//    let (mut acc, mut syms) = eval_ranged_mul(syms)?;
//    while let [Symbol::Minus, ref tail @ ..] = syms {
//        let (val, tail) = eval_ranged_mul(tail)?;
//
//        let corners = [
//            acc.start - val.start,
//            acc.end - val.start,
//            acc.start - val.end,
//            acc.end - val.end,
//        ];
//
//        let min = *corners.iter().min().unwrap();
//        let max = *corners.iter().max().unwrap();
//        acc = min..max;
//
//        syms = tail;
//    }
//
//    Some((acc, syms))
//}
//
//fn eval_ranged_add(syms: &[Symbol]) -> Option<(ValueSet, &[Symbol])> {
//    let (mut acc, mut syms) = eval_ranged_sub(syms)?;
//    while let [Symbol::Plus, ref tail @ ..] = syms {
//        let (val, tail) = eval_ranged_sub(tail)?;
//
//        let corners = [
//            acc.start + val.start,
//            acc.end + val.start,
//            acc.start + val.end,
//            acc.end + val.end,
//        ];
//
//        let min = *corners.iter().min().unwrap();
//        let max = *corners.iter().max().unwrap();
//        acc = min..max;
//
//        syms = tail;
//    }
//
//    Some((acc, syms))
//}

fn eval_ranged(syms: &[Symbol]) -> Option<ValueSet> {
    match eval_ranged_add(syms) {
        Some((range, &[])) => Some(range),
        _ => None,
    }
}

fn possible(syms: &[Symbol]) -> bool {
    let (left, syms) = match eval_ranged_add(syms) {
        Some((left, [Symbol::Equals, tail @ ..])) => (left, tail),
        _ => return false,
    };

    let right = match eval_ranged_digits(syms) {
        Some((right, &[])) => right,
        _ => return false,
    };

    !left.intersection(&right).is_empty()
}

fn _num_length(n: i32) -> usize {
    format!("{n}").len()
}

fn factors(num: u32) -> Vec<u32> {
    (1..num).filter(|d| num % d == 0).collect()
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Word<const N: usize = 8> {
    symbols: [Symbol; N],
}

impl<const N: usize> Word<N> {
    fn new() -> Self {
        Self {
            symbols: [Symbol::Unknown; N],
        }
    }

    fn random() -> Self {
        fn write_symbol(slice: &mut [Symbol], symbol: Symbol) -> Result<&mut [Symbol], ()> {
            match slice {
                [head, tail @ ..] => {
                    *head = symbol;
                    Ok(tail)
                }
                _ => Err(()),
            }
        }

        fn write_number(mut slice: &mut [Symbol], mut num: i32) -> Result<&mut [Symbol], ()> {
            if num == 0 {
                return write_symbol(slice, Symbol::Digit(0));
            }

            if num < 0 {
                slice = write_symbol(slice, Symbol::Minus)?;
                num = -num;
            };

            while num > 0 {
                slice = write_symbol(slice, Symbol::Digit((num % 10) as _))?;
                num /= 10;
            }

            Ok(slice)
        }

        fn random_expr<'s>(
            mut slice: &'s mut [Symbol],
            rng: &mut rand::rngs::ThreadRng,
            target: i32,
            allow_additive: bool,
        ) -> Result<&'s mut [Symbol], ()> {
            if slice.len() < 3 || rng.gen_bool(0.3) {
                return write_number(slice, target);
            } else {
                let choice: u8 = rng.gen_range(match allow_additive {
                    true => 0..4,
                    false => 2..4,
                });

                match choice {
                    0 => {
                        let r = target.abs();
                        let other = rng.gen_range(-r..=r);
                        slice = random_expr(slice, rng, target - other, false)?;
                        slice = write_symbol(slice, Symbol::Plus)?;
                        slice = random_expr(slice, rng, other, false)?;
                    }
                    1 => {
                        let r = target.abs();
                        let other = rng.gen_range(-r..=r);
                        slice = random_expr(slice, rng, target + other, false)?;
                        slice = write_symbol(slice, Symbol::Minus)?;
                        slice = random_expr(slice, rng, other, false)?;
                    }
                    2 => {
                        let factors = factors(target.abs() as _);
                        if factors.is_empty() {
                            return Err(());
                        }

                        let index = rng.gen_range(0..factors.len());
                        let other = factors[index] as _;
                        slice = write_number(slice, target / other)?;
                        slice = write_symbol(slice, Symbol::Times)?;
                        slice = write_number(slice, other)?;
                    }
                    3 => {
                        let r = (target.abs() as f32).sqrt() as i32;
                        let other = rng.gen_range(-r..=r);
                        slice = write_number(slice, target * other)?;
                        slice = write_symbol(slice, Symbol::Slash)?;
                        slice = write_number(slice, other)?;
                    }
                    _ => unreachable!(),
                }
            }

            Ok(slice)
        }

        //fn random_expr_old(slice: &mut [Symbol], rng: &mut rand::rngs::ThreadRng) {
        //    if slice.len() < 3 || rng.gen_bool(0.3) {
        //        for sym in slice {
        //            *sym = Symbol::Digit(rng.gen_range(0..10));
        //        }
        //    } else {
        //        const OPS: [Symbol; 3] = [Symbol::Plus, Symbol::Minus, Symbol::Times];
        //        let op = OPS[rng.gen_range(0..OPS.len())];
        //
        //        let index = rng.gen_range(1..slice.len() - 1);
        //        slice[index] = op;
        //        random_expr_old(&mut slice[..index], rng);
        //        random_expr_old(&mut slice[index + 1..], rng);
        //    }
        //}

        let mut word = Self::new();
        let mut rng = rand::thread_rng();

        let min = -i32::pow(10, N as u32 / 2 - 1);
        let max = i32::pow(10, N as u32 / 2);

        loop {
            let target = rng.gen_range(min..max);
            let mut end = N - 1;

            {
                let mut target = target;
                loop {
                    let digit = target % 10;
                    word.symbols[end] = Symbol::Digit(digit as _);

                    target /= 10;
                    end -= 1;

                    if target == 0 {
                        word.symbols[end] = Symbol::Equals;
                        break;
                    }
                }
            }

            let res = random_expr(&mut word.symbols[..end], &mut rng, target, true);

            if matches!(res, Ok(s) if s.is_empty()) && word.is_valid() {
                break;
            }
        }

        word
    }

    fn is_valid(&self) -> bool {
        valid(&self.symbols).unwrap_or(false)
    }
}

impl<const N: usize> std::fmt::Display for Word<N> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for sym in &self.symbols {
            write!(f, "{sym}")?
        }

        Ok(())
    }
}

impl<const N: usize> std::str::FromStr for Word<N> {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.len() != N {
            return Err(());
        }

        let mut symbols = [Symbol::Times; N];
        for index in 0..symbols.len() {
            let sub = &s[index..=index];
            let symbol: Symbol = sub.parse()?;
            symbols[index] = symbol;
        }

        Ok(Word { symbols })
    }
}

fn main() {
    for _ in 0..1000 {
        let word = Word::<8>::random();
        print!("{word} ");

        let mut stdout = std::io::stdout();
        stdout.flush().unwrap();
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn eval_number() {
        let symbols = [Symbol::Minus, Symbol::Digit(3), Symbol::Digit(2)];
        let value = eval(&symbols);
        assert_eq!(value, Some(-32));
    }

    #[test]
    fn eval_add_simple() {
        let symbols = [Symbol::Digit(2), Symbol::Plus, Symbol::Digit(3)];
        let value = eval(&symbols);
        assert_eq!(value, Some(5));
    }

    #[test]
    fn expr_eval_simple() {
        let word: Word<11> = "10-2*3+8/-4".parse().unwrap();
        let value = eval(&word.symbols);
        assert_eq!(value, Some(2));
    }

    #[test]
    fn word_parse_simple() {
        let text = "2*3-4=2";
        let word: Word<7> = text.parse().unwrap();
        assert_eq!(word.to_string(), text);
    }

    #[test]
    fn word_valid_simple() {
        let text = "2*3-4=2";
        let word: Word<7> = text.parse().unwrap();
        assert_eq!(word.is_valid(), true);
    }

    #[test]
    fn word_invalid_simple() {
        let text = "2*3-4=5";
        let word: Word<7> = text.parse().unwrap();
        assert_eq!(word.is_valid(), false);
    }

    #[test]
    fn eval_ranged_number_trailing_unknown() {
        let symbols = [Symbol::Minus, Symbol::Digit(3), Symbol::Unknown];
        let value = eval_ranged(&symbols).unwrap();
        assert_eq!(value.range(), -39..-30);
    }

    #[test]
    fn eval_ranged_number_middle_unknown() {
        let symbols = [Symbol::Digit(1), Symbol::Unknown, Symbol::Digit(3)];
        let value = eval_ranged(&symbols).unwrap();
        assert_eq!(value.range(), 103..193);
    }

    #[test]
    fn eval_ranged_number_first_unknown() {
        let symbols = [Symbol::Unknown, Symbol::Digit(2), Symbol::Digit(5)];
        let value = eval_ranged(&symbols).unwrap();
        assert_eq!(value.range(), -25..925);
    }

    #[test]
    fn eval_ranged_linear_simple() {
        let symbols = [
            Symbol::Unknown,
            Symbol::Times,
            Symbol::Unknown,
            Symbol::Plus,
            Symbol::Unknown,
        ];
        let value = eval_ranged(&symbols).unwrap();

        // Min: 1 * 1 + 1 = 2
        // Max: 9 * 9 + 9 = 9 * 10 = 90
        assert_eq!(value.range(), 2..90);
    }

    #[test]
    fn eval_ranged_unsigned_division() {
        let symbols = [
            Symbol::Digit(9),
            Symbol::Unknown,
            Symbol::Unknown,
            Symbol::Slash,
            Symbol::Unknown,
        ];
        let value = eval_ranged(&symbols).unwrap();

        // Min: 900 / 9 = 100
        // Max: 999 / 1 = 999
        assert_eq!(value.range(), 100..999);
    }

    #[test]
    fn eval_ranged_signed_division() {
        let symbols = [
            Symbol::Unknown,
            Symbol::Unknown,
            Symbol::Unknown,
            Symbol::Slash,
            Symbol::Unknown,
        ];
        let value = eval_ranged(&symbols).unwrap();

        // Min: -99 / 1 = -99
        // Max: 999 / 1 = 999
        assert_eq!(value.range(), -99..999);
    }

    #[test]
    fn possible_true_simple() {
        let word: Word<8> = "??+??=??".parse().unwrap();
        assert!(possible(&word.symbols));
    }

    #[test]
    fn possible_true_also_simple() {
        let word: Word<8> = "?*?+?=??".parse().unwrap();
        assert!(possible(&word.symbols));
    }

    #[test]
    fn possible_false_simple() {
        let word: Word<8> = "??-?=2??".parse().unwrap();
        assert!(!possible(&word.symbols));
    }

    #[test]
    fn possible_false_also_simple() {
        let word: Word<8> = "7?-2?=3?".parse().unwrap();
        assert!(!possible(&word.symbols));
    }
}

//enum Op {
//    Add,
//    Sub,
//    Mul,
//    Div,
//}
//
//impl std::fmt::Display for Op {
//    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//        let s = match self {
//            Op::Add => "+",
//            Op::Sub => "-",
//            Op::Mul => "*",
//            Op::Div => "/",
//        };
//
//        write!(f, "{s}")
//    }
//}
//
//enum Expr {
//    Number(i32),
//    Binary(Op, Box<Expr>, Box<Expr>),
//    Negate(Box<Expr>),
//}
//
//impl Expr {
//    fn eval(&self) -> Option<i32> {
//        match self {
//            Self::Number(n) => Some(*n),
//
//            Self::Binary(op, left, right) => {
//                let left = left.eval()?;
//                let right = right.eval()?;
//
//                match op {
//                    Op::Add => left.checked_add(right),
//                    Op::Sub => left.checked_sub(right),
//                    Op::Mul => left.checked_mul(right),
//                    Op::Div => left.checked_div(right),
//                }
//            }
//
//            Self::Negate(expr) => expr.eval()?.checked_neg(),
//        }
//    }
//}
//
//impl FromStr for Expr {
//    type Err = ();
//
//    fn from_str(s: &str) -> Result<Self, Self::Err> {
//        fn parse_number(src: &str) -> Option<(Expr, &str)> {
//            let neg = src.starts_with('-');
//            let src = &src[neg as usize..];
//
//            let len = src.find(|c| !matches!(c, '0'..='9')).unwrap_or(src.len());
//            let (num, r) = src.split_at(len);
//            let mut num: i32 = num.parse().ok()?;
//            if neg {
//                num = num.checked_neg()?;
//            }
//
//            Some((Expr::Number(num), r))
//        }
//
//        fn parse_mul(src: &str) -> Option<(Expr, &str)> {
//            let (left, src) = parse_number(src)?;
//            if src.starts_with('*') {
//                let (right, src) = parse_mul(&src[1..])?;
//                return Some((Expr::Binary(Op::Mul, Box::new(left), Box::new(right)), src));
//            }
//            Some((left, src))
//        }
//
//        fn parse_div(src: &str) -> Option<(Expr, &str)> {
//            let (left, src) = parse_mul(src)?;
//            if src.starts_with('/') {
//                let (right, src) = parse_div(&src[1..])?;
//                return Some((Expr::Binary(Op::Div, Box::new(left), Box::new(right)), src));
//            }
//            Some((left, src))
//        }
//
//        fn parse_sub(src: &str) -> Option<(Expr, &str)> {
//            let (left, src) = parse_div(src)?;
//            if src.starts_with('-') {
//                let (right, src) = parse_sub(&src[1..])?;
//                return Some((Expr::Binary(Op::Sub, Box::new(left), Box::new(right)), src));
//            }
//            Some((left, src))
//        }
//
//        fn parse_add(src: &str) -> Option<(Expr, &str)> {
//            let (left, src) = parse_sub(src)?;
//            if src.starts_with('+') {
//                let (right, src) = parse_add(&src[1..])?;
//                return Some((Expr::Binary(Op::Add, Box::new(left), Box::new(right)), src));
//            }
//            Some((left, src))
//        }
//
//        let (expr, _) = parse_add(s).ok_or(())?;
//        Ok(expr)
//    }
//}
//
//impl std::fmt::Display for Expr {
//    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//        match self {
//            Expr::Number(n) => write!(f, "{}", n),
//            Expr::Binary(op, left, right) => write!(f, "{}{}{}", left, op, right),
//            Expr::Negate(expr) => write!(f, "-{}", expr),
//        }
//    }
//}
