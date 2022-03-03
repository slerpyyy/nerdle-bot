#![allow(dead_code)]

use std::{
    collections::HashMap,
    io::Write,
    mem::MaybeUninit, sync::atomic::{AtomicUsize, Ordering},
};

use rayon::prelude::*;

//use hint::*;
use word::*;

mod domain;
mod hint;
mod word;

const N: usize = 8;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Color {
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

fn compute_entropy<const N: usize>(guess: Word<N>, word_list: &[Word<N>]) -> f32 {
    let mut buckets = HashMap::<[Color; N], usize>::new();
    for word in word_list {
        let hint = compare_words(&guess, word);

        match buckets.get_mut(&hint) {
            Some(bucket) => *bucket += 1,
            None => drop(buckets.insert(hint, 1)),
        }
    }

    let total = word_list.len() as f32;
    let mut entropy = 0.0;

    for count in buckets.into_values().filter(|&v| v > 0) {
        let bucket_probability = count as f32 / total;
        let bucket_entropy = -bucket_probability.log2();
        debug_assert!(bucket_entropy.is_finite());

        entropy += bucket_probability * bucket_entropy;
    }

    entropy
}

fn main() {
    println!("Generating words");
    let words = {
        let mut acc = Vec::new();
        let mut word = Word::<N>::new();

        word.query(
            |_word| true,
            |word| {
                acc.push(word.clone());
            },
        );

        acc
    };
    let total = words.len();
    println!(" | total words: {total}");

    println!("Computing entropy");
    let counter = AtomicUsize::new(0);
    let mut entropy_values: Vec<_> = words
        .par_iter()
        .map(|guess| compute_entropy(guess.clone(), &words))
        .enumerate()
        .inspect(|_| {
            let k = counter.fetch_add(1, Ordering::AcqRel);
            if k % 100 == (total - 1) % 100 {
                let progress = k as f32 / total as f32;
                print!("\r | progress: {:.2}%", 100.0 * progress);
                std::io::stdout().flush().unwrap();
            }
        })
        .collect();
    print!("\n");

    println!("Sort by entropy");
    entropy_values.sort_by(|(_, a), (_, b)| a.partial_cmp(b).unwrap().reverse());

    for (index, entropy) in entropy_values.iter().take(10) {
        println!(" | {} ({:.3} bits)", words[*index], entropy);
    }
}
