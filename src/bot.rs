use std::{
    collections::{BinaryHeap, HashMap},
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

pub fn compare_words<const N: usize>(guess: &Word<N>, answer: &Word<N>) -> [Color; N] {
    let mut hints: [Color; N] = [Color::Black; N];
    let mut answer = answer.clone();

    for index in 0..N {
        if guess[index] == answer[index] {
            hints[index] = Color::Green;
            answer[index] = Symbol::Unknown;
        }
    }

    for index in 0..N {
        if hints[index] == Color::Green {
            continue;
        }

        let guess_sym = &guess[index];
        if let Some(answer_sym) = answer.iter_mut().find(|s| s == &guess_sym) {
            hints[index] = Color::Purple;
            *answer_sym = Symbol::Unknown;
        }
    }

    hints
}

pub fn compute_entropy<const N: usize>(guess: &Word<N>, word_list: &[Word<N>]) -> NotNan<f32> {
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

#[derive(Debug, Clone)]
pub struct Bot<const N: usize> {
    pub words: Vec<Word<N>>,
    pub answers: Vec<Word<N>>,
}

impl<const N: usize> Bot<N> {
    pub fn init() -> Self {
        let mut words = Vec::new();
        let mut base = Word::<N>::new();
        base.search(|word| {
            words.push(word.clone());
        });

        words.sort();

        let answers = words
            .iter()
            .filter(|word| word.is_valid_answer())
            .cloned()
            .collect();

        Self { words, answers }
    }

    pub fn register_guess(&mut self, guess: Word<N>, hints: [Color; N]) {
        self.answers
            .retain(|word| word != &guess && compare_words(&guess, word) == hints);
    }

    pub fn request_guesses(&self) -> impl Iterator<Item = (NotNan<f32>, Word<N>)> {
        let total = self.words.len();
        let counter = AtomicUsize::new(0);

        let mut heap: BinaryHeap<_> = self
            .words
            .par_iter()
            .map(|guess| {
                let entropy = compute_entropy(guess, &self.answers);
                (entropy, guess.clone())
            })
            .filter(|(entropy, guess)| {
                if entropy.into_inner() > 0.0 {
                    return true;
                }

                self.answers.binary_search(guess).is_ok()
            })
            .inspect(|_| {
                let k = counter.fetch_add(1, Ordering::Relaxed);
                if k % 100 == 0 {
                    let progress = k as f32 / total as f32;
                    print!("\r | progress: {:.2}%", 100.0 * progress);
                    std::io::Write::flush(&mut std::io::stdout()).unwrap();
                }
            })
            .collect();

        println!("\r | progress: 100.00%");

        std::iter::from_fn(move || heap.pop())
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn bot_init() {
        let nerd = Bot::<5>::init();

        for word in &nerd.answers {
            print!("{word} ")
        }

        println!("\n----");

        for word in &nerd.words {
            if nerd.answers.contains(word) {
                continue;
            }

            print!("{word} ")
        }

        print!("\n");
    }

    #[test]
    fn tiny_game() {
        let solution: Word<5> = "2*3=6".parse().unwrap();

        let mut nerd = Bot::init();
        let mut tries = 0;

        while let Some((entropy, guess)) = nerd.request_guesses().next() {
            println!(" -> {guess} ({entropy:.3})");

            let hints = compare_words(&guess, &solution);
            nerd.register_guess(guess, hints);
            tries += 1;

            assert!(tries < 6);
        }
    }

    #[test]
    fn small_game() {
        let solution: Word<6> = "3*5=15".parse().unwrap();

        let mut nerd = Bot::init();
        let mut tries = 0;

        while let Some((_entropy, guess)) = nerd.request_guesses().next() {
            println!(" -> {guess}");

            let hints = compare_words(&guess, &solution);
            nerd.register_guess(guess, hints);
            tries += 1;

            assert!(tries < 6);
        }
    }
}
