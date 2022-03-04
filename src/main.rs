use std::io::Write;

use bot::{Bot, Color};
use word::Word;

mod bot;
mod domain;
mod word;

const N: usize = 8;

fn main() {
    let mut bot = Bot::<N>::init();

    while !bot.words.is_empty() {
        println!("Computing entropy");
        let iter = bot.request_guesses();

        for (entropy, word) in iter.take(10) {
            println!(" | {word} ({entropy:.3} bits)");
        }

        println!("Dialog");

        let guess: Word<N> = {
            print!(" | guess: ");
            std::io::stdout().flush().unwrap();

            let mut input = String::new();
            std::io::stdin().read_line(&mut input).unwrap();
            input[..N].parse().unwrap()
        };

        let hints: [Color; N] = {
            print!(" | hints: ");
            std::io::stdout().flush().unwrap();

            let mut input = String::new();
            std::io::stdin().read_line(&mut input).unwrap();

            let mut hints = [Color::Black; N];
            for (char, hint) in input.chars().zip(&mut hints) {
                *hint = match char {
                    'b' => Color::Black,
                    'p' => Color::Purple,
                    'g' => Color::Green,
                    _ => panic!("idk what {char:?} is"),
                };
            }

            hints
        };

        bot.register_guess(guess, hints);
    }
}
