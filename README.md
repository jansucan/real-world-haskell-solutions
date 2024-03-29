# Real World Haskell solutions

These are solutions to exercises from me reading the first edition of the [the
Real World Haskell book](https://book.realworldhaskell.org/). I hope they will
be helpful in your journey to the world of Haskell.

The goal is to provide solutions to all the exercises. I'm adding them gradually
as I'm progressing through the book.

There is often more than one solution to an exercise. If your solution is
different but meets the assignment, it is the right one.

The intention is for the exercises to be solved as by a beginner, with all the
things that a seasoned Haskell programmer would have done differently. The
solutions should mostly use only knowledge from the preceding parts of the
book. Exceptions to this are cases where such approach would lead to too many
additional lines of code or to distracting the reader from the main idea of an
exercise. Advanced topics from the subsequent chapters should be avoided,
though.

## Exercises

There are a few kinds of exercises:

- Describe what happens when you run the code
- Answer a question
- Write an implementation

Solutions to the first two kinds of exercises are provided in comments in
Haskell source files.

### Ambiguity and mistakes in the assignments

Assignments of the exercises are taken exactly as they are in the printed
version of the book without correcting any mistakes. Those are corrected in
comments immediately following the assignments.

Some exercises are unclear or they seem to contain contradicting information. In
these cases I describe my additional assumptions and thought process in
comments.

### Solutions using code from the examples

Some solution implementations use [the example code from the
book](https://resources.oreilly.com/examples/9780596514983). The complete
examples repository these solutions are base on is added as a submodule in the
'examples' directory.

If a solution of an exercise depends on more source code from the examples, the
whole files are copied to a directory of that solution.

If not much code is needed, then just the required parts are copied directly to
the file with implementation and marked by comments.

### Platform

The solutions are implemented and tested in a Linux environment. This also
includes solutions that could have been made multiplatform.

### List of the exercises

Format of an exercise label is '{Module_}\<chapter\>\_\<group\>\_\<exercise\>'. Some
chapters contain more groups of exercises. To differentiate those, an exercise
group letter is included (the exact letters don't have a connection to the
book). To make the exercise groups more visible in the list the first exercise
of a group is in bold italics.

If a solution is intended to be used in solutions to other exercises, to reduce code
duplication, it is imported as a Haskell module. For this to work, the naming
conventions for Haskell modules need to be followed. The names of such source files
are prefixed with 'Module_'.


| Exercise       | Solved | Page | Chapter |
| -------------- | ------ | ---- | ------- |
| **_1_a_1_**    | yes    | 16   | 1. Getting started |
| 1_a_2          | yes    |      | |
| 1_a_3          | yes    |      | |
| 1_a_4          | yes    |      | |
| **_2_a_1_**    | yes    | 25   | 2. Types and functions |
| **_2_b_1_**    | yes    | 39   | |
| 2_b_2          | yes    |      | |
| 2_b_3          | yes    |      | |
| **_3_a_1_**    | yes    | 60   | 3. Defining types, streamlining functions |
| 3_a_2          | yes    |      | |
| **_3_b_1_**    | yes    | 69   | |
| 3_b_2          | yes, in 3_b_1 |      | |
| 3_b_3          | yes    |      | |
| 3_b_4          | yes    |      | |
| 3_b_5          | yes    |      | |
| 3_b_6          | yes    | 70   | |
| 3_b_7          | yes    |      | |
| 3_b_8          | yes    |      | |
| 3_b_9          | yes    |      | |
| 3_b_10         | yes, in 3_b_9 |      | |
| 3_b_11         | yes, in 3_b_9 |      | |
| 3_b_12         | yes, in 3_b_9 |      | |
| **_4_a_1_**    | yes    | 84   | 4. Functional programming |
| 4_a_2          | yes    |      | |
| 4_a_3          | yes    |      | |
| 4_a_4          | yes    |      | |
| **_4_b_1_**    | yes, in 4_b_2 | 97   | |
| 4_b_2          | yes    | 98   | |
| 4_b_3          | yes    |      | |
| 4_b_4          | yes    |      | |
| 4_b_5          | yes, in 4_b_6 |      | |
| 4_b_6          | yes    |      | |
| 4_b_7          | yes    |      | |
| 4_b_8          | yes, in 4_b_9 |      | |
| 4_b_9          | yes    |      | |
| 4_b_10         | yes    |      | |
| **_5_a_1_**    | yes    | 130  | 5. Writing a library: working with JSON data |
| 5_a_2          | yes, in 5_a_1 |      | |
| **_6_a_1_**    | yes    | 162  | 6. Using typeclasses |
| 6_a_2          | yes    |      | |
| **_8_a_1_**    | yes    | 205  | 8. Efficient file processing, regular expressions, and file name matching |
| 8_a_2          | yes    |      | |
| **_8_b_1_**    | yes, in 8_a_2 | 210  | |
| 8_b_2          | yes    |      | |
| 8_b_3          | yes    |      | |
| **_Module_8_c_1_** | yes    | 211  | |
| 8_c_2          | yes    |      | |
| **_8_d_1_**    | yes    | 212  | |
| **_9_a_1_**    | yes    | 221  | 9. I/O case study: a library for searching the filesystem |
| **_9_b_1_**    | yes    | 228  | |
| 9_b_2          | yes    |      | |
| Module_9_b_3   | yes    |      | |
| 9_b_4          | yes    |      | |
| **_9_c_1_**    | yes    | 232  | |
| 9_c_2          | yes    |      | |
| 9_c_3          | yes    |      | |
| **_9_d_1_**    | yes    | 234  | |
| 9_d_2          | yes    |      | |
| **_10_a_1_**   | yes    | 254  | 10. Code case study: parsing a binary data format |
| 10_a_2         | yes, in 10_a_1  |      | |
| 10_a_3         | yes, in 10_a_1  |      | |
| **_12_a_1_**   | yes    | 274  | 12. Barcode recognition |
| 12_a_2         | yes, in 12_a_1  |      | |
| 12_a_3         | yes, in 12_a_1  |      | |
| **_13_a_1_**   | yes    | 316  | 13. Data structures |
| **_14_a_1_**   | yes    | 352  | 14. Monads |
| **_15_a_1_**   |        | 382  | 15. Programming with monads |
| 15_a_2         |        |      | |
| 15_a_3         |        |      | |
| **_16_a_1_**   |        | 403  | 16. The Parsec parsing library |
| 16_a_2         |        |      | |
| 16_a_3         |        |      | |
| 16_a_4         |        |      | |
| **_18_a_1_**   |        | 436  | 18. Monad transformers |
| 18_a_2         |        |      | |
| 18_a_3         |        |      | |
| **_18_b_1_**   |        | 441  | |
| **_19_a_1_**   |        | 462  | 19. Error handling |
| **_19_b_1_**   |        | 465  | |
| 19_b_2         |        |      | |
| 19_b_3         |        |      | |
| **_23_a_1_**   |        | 529  | 23. GUI programming |
| 23_a_2         |        |      | |
| 23_a_3         |        |      | |
| **_24_a_1_**   |        | 542  | 24. Basic concurrent and parallel programming |
| 24_a_2         |        |      | |
| **_24_b_1_**   |        | 551  | |
| 24_b_2         |        |      | |
| **_26_a_1_**   |        | 610  | 26. Advanced library design: building a Bloom filter |
| 26_a_2         |        |      | |

## Bug reports

If you find an error in a solution or anything that should be described more
clearly, feel free to let me know by opening an issue here or a pull request
here.
