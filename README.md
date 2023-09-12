# nerdle-bot
*A solver for the game Nerdle*

This is a solver for the game [Nerdle](https://nerdlegame.com/), a Wordle clone where instead of guessing 5-letter words, you guess a valid 8-character mathematical equation.
The bot uses a similar approach as the one described in the famous [Solving Wordle using information theory](https://www.youtube.com/watch?v=v68zYyaEmEA) video by [3Blue1Brown](https://www.youtube.com/@3blue1brown).

The thing that sets this project appart from other wordle solvers is that there is no official wordlist for Nerdle, meaning the exhaustive list of possible equations must be computed first.
This is done using a recusive tree search with backtracking (similar to how you'd solve a sudoku), i.e. the word starts as `????????`, then we place the `=` sign, then some operators and finally fill in the numbers.
Words are evaluated and validated using a simple pecedence-climbing recursive descent parser.
Branches are pruned early using range evaluation.

## Usage

After starting the program, the bot computes the entropy for each guess and prints out a ranking of valid guesses, listed best to worst.
Once you made your guess in Nerdle, you report back the guess you chose and the color hints you got back (**b**lack, **p**urple, or **g**reen).

Here is a log of todays Nerdle (12.09.2023):

```
Current counts
 | words:   135975
 | answers: 17171 
Computing entropy 
 | progress: 100.00%
Ranking
 | 48-32=16 (9.779 bits)
 | 43-25=18 (9.771 bits)
 | 48-36=12 (9.769 bits)
 | 45-27=18 (9.762 bits)
 | 52-34=18 (9.762 bits)
 | 46-27=19 (9.760 bits)
 | 46-29=17 (9.758 bits)
 | 45-29=16 (9.753 bits)
 | 43-27=16 (9.753 bits)
 | 52-38=14 (9.750 bits)
Dialog
 | guess: 48-32=16
 | hints: pppbppbb
Current counts    
 | words:   135975
 | answers: 22    
Computing entropy 
 | progress: 100.00%    
Ranking
 | 8*04/4=8 (4.369 bits)
 | 7-24/8=4 (4.369 bits)
 | 7-08/4=5 (4.369 bits)
 | 8*74=592 (4.369 bits)
 | 8--4-8=4 (4.369 bits)
 | 84/7-8=4 (4.369 bits)
 | 8*54=432 (4.278 bits)
 | 8-7+04=5 (4.278 bits)
 | 8-0*74=8 (4.278 bits)
 | 8-08+4=4 (4.278 bits)
Dialog
 | guess: 8*74=592
 | hints: gbbgpppg
Current counts
 | words:   135975
 | answers: 1
Computing entropy
 | progress: 100.00%
The answer is: 8-54/9=2
```
