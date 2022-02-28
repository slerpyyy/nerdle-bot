#![allow(dead_code)]

use std::time::Instant;

use word::*;

mod domain;
mod word;

fn main() {
    let start = Instant::now();
    let mut count = 0;

    Word::<8>::solve(|_word| {
        count += 1;
    });

    let time = start.elapsed();
    println!("Found {count} words in {time:?}");
}
