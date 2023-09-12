use crate::domain::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Symbol {
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
            Symbol::Digit(d) => return write!(f, "{d}"),
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

#[derive(Debug)]
pub struct SymbolParseError {
    pub input: String,
    pub position: usize,
}

impl std::str::FromStr for Symbol {
    type Err = SymbolParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.as_bytes() {
            &[d @ b'0'..=b'9'] => Ok(Symbol::Digit((d - b'0') as _)),
            &[b'+'] => Ok(Symbol::Plus),
            &[b'-'] => Ok(Symbol::Minus),
            &[b'*'] => Ok(Symbol::Times),
            &[b'/'] => Ok(Symbol::Slash),
            &[b'='] => Ok(Symbol::Equals),
            &[b'?'] => Ok(Symbol::Unknown),
            _ => Err(SymbolParseError {
                input: s.to_string(),
                position: 0,
            }),
        }
    }
}

fn eval_ranged_digits_unsigned(mut symbols: &[Symbol]) -> Option<(Domain, &[Symbol])> {
    if !matches!(symbols, &[Symbol::Digit(_) | Symbol::Unknown, ..]) {
        return None;
    }

    let mut min_acc: i32 = 0;
    let mut max_acc: i32 = 0;

    //let mut first_digit = true;
    while let [digit, tail @ ..] = symbols {
        let (min_digit, max_digit) = match *digit {
            //Symbol::Digit(0) if first_digit => return None,
            Symbol::Digit(d) => (d, d),
            //Symbol::Unknown if first_digit => (1, 9),
            Symbol::Unknown => (0, 9),
            _ => break,
        };

        min_acc = min_acc * 10 + (min_digit as i32);
        max_acc = max_acc * 10 + (max_digit as i32);

        symbols = tail;
        //first_digit = false;
    }

    let range = Interval::new(min_acc, max_acc);
    let set = Domain::from_range(range);
    Some((set, symbols))
}

fn eval_ranged_digits(symbols: &[Symbol]) -> Option<(Domain, &[Symbol])> {
    match symbols {
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
    fn $name(symbols: &[Symbol]) -> Option<(Domain, &[Symbol])> {
        let (mut left, mut symbols) = $recurse(symbols)?;
        while let [Symbol::$symbol, ref tail @ ..] = symbols {
            let (right, tail) = $recurse(tail)?;
            left = left $op right;
            if left.is_empty() {
                return None;
            }
            symbols = tail;
        }

        Some((left, symbols))
    }
};
}

impl_eval_ranged_op!(eval_ranged_div, /, Slash, eval_ranged_digits);
impl_eval_ranged_op!(eval_ranged_mul, *, Times, eval_ranged_div);
impl_eval_ranged_op!(eval_ranged_sub, -, Minus, eval_ranged_mul);
impl_eval_ranged_op!(eval_ranged_add, +, Plus,  eval_ranged_sub);

#[cfg(test)]
fn eval_ranged(symbols: &[Symbol]) -> Option<Domain> {
    match eval_ranged_add(symbols) {
        Some((range, &[])) => Some(range),
        _ => None,
    }
}

#[cfg(test)]
fn eval(symbols: &[Symbol]) -> Option<i32> {
    let interval = eval_ranged(symbols)?.range();
    match interval.len() {
        1 => Some(interval.start),
        _ => None,
    }
}

fn possible(symbols: &[Symbol]) -> bool {
    if symbols.first() == Some(&Symbol::Minus) {
        return false;
    }

    let (left, symbols) = match eval_ranged_add(symbols) {
        Some((left, [Symbol::Equals, tail @ ..])) => (left, tail),
        _ => return false,
    };

    let right = match eval_ranged_digits(symbols) {
        Some((right, &[])) => right,
        _ => return false,
    };

    //println!("{left} ?= {right}");

    !left.intersection(&right).is_empty()
}

fn valid_answer(symbols: &[Symbol]) -> bool {
    let double_ops = !symbols
        .iter()
        .skip(1)
        .zip(symbols)
        .all(|(a, b)| matches!(a, &Symbol::Digit(_)) || matches!(b, &Symbol::Digit(_)));

    if double_ops {
        return false;
    }

    let eq_index = match symbols.iter().position(|s| s == &Symbol::Equals) {
        Some(index) => index,
        None => return false,
    };

    let left = &symbols[..eq_index];
    let right = &symbols[(eq_index + 1)..];

    let valid_right = match right {
        [] => false,
        [Symbol::Digit(_)] => true,
        [Symbol::Digit(0), ..] => false,
        s => s.iter().all(|s| matches!(s, Symbol::Digit(_))),
    };

    if !valid_right {
        return false;
    }

    let valid_left = {
        let mut valid = true;
        let mut allow_zero = false;

        for s in left {
            match s {
                Symbol::Digit(0) if !allow_zero => {
                    valid = false;
                    break;
                }

                Symbol::Digit(_) => allow_zero = true,
                _ => allow_zero = false,
            }
        }

        valid
    };

    valid_left
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Word<const N: usize = 8> {
    pub symbols: [Symbol; N],
}

impl<const N: usize> std::ops::Deref for Word<N> {
    type Target = [Symbol];

    fn deref(&self) -> &Self::Target {
        &self.symbols
    }
}

impl<const N: usize> std::ops::DerefMut for Word<N> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.symbols
    }
}

impl<const N: usize> AsRef<[Symbol]> for Word<N> {
    fn as_ref(&self) -> &[Symbol] {
        &self.symbols
    }
}

impl<const N: usize> Word<N> {
    pub fn new() -> Self {
        Self {
            symbols: [Symbol::Unknown; N],
        }
    }

    pub fn is_possible(&self) -> bool {
        possible(&self.symbols)
    }

    #[cfg(test)]
    pub fn is_valid(&self) -> bool {
        self.symbols.iter().all(|sym| sym != &Symbol::Unknown) && self.is_possible()
    }

    pub fn is_valid_answer(&self) -> bool {
        valid_answer(&self)
    }

    fn search_digits(&mut self, observer: &mut impl FnMut(&Self)) {
        if !self.is_possible() {
            return;
        }

        if let Some(index) = self.symbols.iter().position(|s| s == &Symbol::Unknown) {
            for digit in 0..10 {
                self.symbols[index] = Symbol::Digit(digit);
                self.search_digits(observer);
            }

            self.symbols[index] = Symbol::Unknown;
        } else {
            observer(self)
        }
    }

    fn search_ops(&mut self, observer: &mut impl FnMut(&Self), start: usize, depth: u16) {
        self.search_digits(observer);

        if depth > 0 {
            for index in start..N {
                if self.symbols[index] != Symbol::Unknown {
                    continue;
                }

                for op in [Symbol::Plus, Symbol::Minus, Symbol::Times, Symbol::Slash] {
                    self.symbols[index] = op;
                    self.search_ops(observer, index + 1, depth - 1);
                }

                self.symbols[index] = Symbol::Unknown;
            }
        }
    }

    pub fn search(&mut self, mut observer: impl FnMut(&Self)) {
        let max_ops = N / 2;
        if self.contains(&Symbol::Equals) {
            self.search_ops(&mut observer, 0, max_ops as _);
        } else {
            for index in 1..N.saturating_sub(1) {
                self.symbols[index] = Symbol::Equals;
                self.search_ops(&mut observer, 0, max_ops as _);
                self.symbols[index] = Symbol::Unknown;
            }
        }
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
    type Err = SymbolParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.len() != N {
            return Err(SymbolParseError {
                input: s.to_string(),
                position: N,
            });
        }

        let mut symbols = [Symbol::Times; N];
        for index in 0..symbols.len() {
            let sub = &s[index..=index];
            let symbol: Symbol = sub.parse().map_err(|mut err: Self::Err| {
                err.position = index;
                err
            })?;

            symbols[index] = symbol;
        }

        Ok(Word { symbols })
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
    fn eval_ranged_no_uncertainty() {
        let symbols = [
            Symbol::Digit(1),
            Symbol::Digit(1),
            Symbol::Plus,
            Symbol::Digit(5),
        ];
        let value = eval_ranged(&symbols).unwrap();
        assert_eq!(value.range(), Interval::new(16, 16));
    }

    #[test]
    fn eval_ranged_number_trailing_unknown() {
        let symbols = [Symbol::Minus, Symbol::Digit(3), Symbol::Unknown];
        let value = eval_ranged(&symbols).unwrap();
        assert_eq!(value.range(), Interval::new(-39, -30));
    }

    #[test]
    fn eval_ranged_number_middle_unknown() {
        let symbols = [Symbol::Digit(1), Symbol::Unknown, Symbol::Digit(3)];
        let value = eval_ranged(&symbols).unwrap();
        assert_eq!(value.range(), Interval::new(103, 193));
    }

    #[test]
    fn eval_ranged_number_first_unknown() {
        let symbols = [Symbol::Unknown, Symbol::Digit(2), Symbol::Digit(5)];
        let value = eval_ranged(&symbols).unwrap();
        assert_eq!(value.range(), Interval::new(-25, 925));
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

        // Min: 0 * 0 + 0 = 0
        // Max: 9 * 9 + 9 = 9 * 10 = 90
        assert_eq!(value.range(), Interval::new(0, 90));
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
        assert_eq!(value.range(), Interval::new(100, 999));
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
        assert_eq!(value.range(), Interval::new(-99, 999));
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

    #[test]
    fn possible_true_tricky() {
        let word: Word<8> = "??/?=???".parse().unwrap();
        assert!(possible(&word.symbols));
    }
}
