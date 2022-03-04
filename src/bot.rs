use std::{
    collections::{BinaryHeap, HashMap},
    io::Write,
    mem::MaybeUninit,
    sync::atomic::{AtomicUsize, Ordering},
};

use ordered_float::NotNan;
use rayon::prelude::*;

use crate::word::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Color {
    Green,
    Purple,
    Black,
}

fn compare_words<const N: usize>(guess: &Word<N>, solution: &Word<N>) -> [Color; N] {
    // SAFETY: An uninitialized `[MaybeUninit<_>; _]` is valid.
    let mut hints: [MaybeUninit<_>; N] = unsafe { MaybeUninit::uninit().assume_init() };

    for index in 0..N {
        let curr = guess[index];

        if curr == solution[index] {
            hints[index].write(Color::Green);
            continue;
        }

        let occ_in_solution = solution.iter().filter(|&s| s == &curr).count();
        let occ_in_guess = guess.iter().take(index).filter(|&s| s == &curr).count();

        if occ_in_solution > occ_in_guess {
            hints[index].write(Color::Purple);
            continue;
        }

        hints[index].write(Color::Black);
    }

    // SAFETY:
    // * All elements of the array are initialized
    // * `MaybeUninit<T>` and `T` are guaranteed to have the same layout
    // * `MaybeUninit` does not drop, so there are no double-frees
    unsafe { (&hints as *const _ as *const [_; N]).read() }
}

fn compute_entropy<const N: usize>(guess: &Word<N>, word_list: &[Word<N>]) -> NotNan<f32> {
    let mut buckets = HashMap::<[Color; N], usize>::new();
    for word in word_list {
        let hint = compare_words(guess, word);

        match buckets.get_mut(&hint) {
            Some(bucket) => *bucket += 1,
            None => drop(buckets.insert(hint, 1)),
        }
    }

    let total = word_list.len() as f32;
    let mut acc = NotNan::new(0.0).unwrap();

    for count in buckets.into_values().filter(|&v| v > 0) {
        let probability = count as f32 / total;
        let entropy = -probability * probability.log2();

        if let Ok(val) = NotNan::new(entropy) {
            acc += val;
        }
    }

    acc
}

pub struct Bot<const N: usize> {
    pub words: Vec<Word<N>>,
}

impl<const N: usize> Bot<N> {
    pub fn init() -> Self {
        let mut words = Vec::new();
        let mut base = Word::<N>::new();
        base.query(
            |_word| true,
            |word| {
                words.push(word.clone());
            },
        );

        Self { words }
    }

    pub fn register_guess(&mut self, guess: Word<N>, hints: [Color; N]) {
        self.words
            .retain(|word| word != &guess && compare_words(&guess, word) == hints);
    }

    pub fn request_guesses(&self) -> impl Iterator<Item = (NotNan<f32>, Word<N>)> {
        let total = self.words.len();
        let counter = AtomicUsize::new(0);

        let mut heap: BinaryHeap<_> = self
            .words
            .par_iter()
            .map(|guess| {
                let entropy = compute_entropy(guess, &self.words);
                (entropy, guess.clone())
            })
            .inspect(|_| {
                let k = counter.fetch_add(1, Ordering::AcqRel);
                if (total - k) % 100 == 0 {
                    let progress = k as f32 / total as f32;
                    print!("\r | progress: {:.2}%", 100.0 * progress);
                    std::io::stdout().flush().unwrap();
                }
            })
            .collect();

        std::iter::from_fn(move || heap.pop())
    }
}
