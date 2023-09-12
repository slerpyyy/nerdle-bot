use std::io::Write;

use bot::{Bot, Color};
use word::Word;

mod bot;
mod domain;
mod word;

const N: usize = 8;

fn main() {
    let mut bot = Bot::<N>::init();

    loop {
        println!("Current counts");
        println!(" | words:   {}", bot.words.len());
        println!(" | answers: {}", bot.answers.len());

        println!("Computing entropy");
        let iter = bot.request_guesses();
        let guesses: Vec<_> = iter.take(10).collect();

        if guesses.is_empty() {
            println!("No more guesses!\n");
            println!("Every possible answer has been ruled out.");
            println!("This should never happen.");
            println!("Make sure you transcribed the guesses and colors correctly.");
            return;
        }

        if let [(_, guess)] = &guesses[..] {
            println!("The answer is: {guess}");
            return;
        }

        println!("Ranking");
        for (entropy, word) in guesses {
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

            if hints.iter().all(|h| h == &Color::Green) {
                println!("What a great guess!");
                return;
            }

            hints
        };

        bot.register_guess(guess, hints);
    }
}
