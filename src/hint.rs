use std::mem::MaybeUninit;

use crate::word::{Symbol, Word};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Hint {
    Green(Symbol, usize),
    Purple(Symbol, usize),
    Black(Symbol),
}

impl Hint {
    pub fn accepts(&self, word: impl AsRef<[Symbol]>) -> bool {
        let word = word.as_ref();

        match self {
            Hint::Green(hint_sym, index) => word[*index] == *hint_sym,
            Hint::Purple(hint_sym, index) => word[*index] != *hint_sym && word.contains(hint_sym),
            Hint::Black(hint_sym) => !word.contains(hint_sym),
        }
    }

    pub fn iter_all<const N: usize>(word: Word<N>) -> Iter<N> {
        assert!(N < u32::MAX as _);
        assert_ne!(3_u64.checked_pow(N as _), None);

        Iter { word, counter: 0 }
    }
}

pub struct Iter<const N: usize> {
    word: Word<N>,
    counter: u64,
}

impl<const N: usize> Iterator for Iter<N> {
    type Item = [Hint; N];

    fn next(&mut self) -> Option<Self::Item> {
        // SAFETY: An uninitialized `[MaybeUninit<_>; _]` is valid.
        let mut out: [MaybeUninit<Hint>; N] = unsafe { MaybeUninit::uninit().assume_init() };

        let mut number = self.counter;
        for (index, item) in out.iter_mut().enumerate() {
            let sym = self.word[index];
            let color = number % 3;

            item.write(match color {
                0 => Hint::Green(sym, index),
                1 => Hint::Purple(sym, index),
                2 => Hint::Black(sym),
                _ => unreachable!(),
            });

            number /= 3;
        }

        if number > 0 {
            return None;
        }

        self.counter += 1;

        // SAFETY:
        // * All elements of the array are initialized
        // * `MaybeUninit<T>` and `T` are guaranteed to have the same layout
        // * `MaybeUninit` does not drop, so there are no double-frees
        let out = unsafe { (&out as *const _ as *const Self::Item).read() };

        Some(out)
    }
}
