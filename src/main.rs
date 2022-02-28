mod domain;
mod word;

use word::*;

fn main() {
    let mut count = 0;

    Word::<5>::solve(|word| {
        print!("{word} ");
        count += 1;
    });

    println!("Done!\n");
    println!("Total: {count}")
}
