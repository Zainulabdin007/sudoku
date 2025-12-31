# sudoku
This project implements a Sudoku solver and validator in Racket using a functional programming style.
It follows common course constraints (no mutation, heavy use of recursion and local, no forbidden helpers), and is fully tested using check-expect.

Features

Validates completed Sudoku boards

Solves solvable Sudoku puzzles

Detects invalid or unsolvable boards

Uses constraint propagation (singles elimination)

Fully functional (no mutation)

Extensive test coverage

Board Representation

A Sudoku board is a 9×9 list of lists

Each cell contains:

A number 1–9 for fixed values

A list of possible values for undecided cells (during solving)

Example solved board:

'((5 3 4 6 7 8 9 1 2)
  (6 7 2 1 9 5 3 4 8)
  (1 9 8 3 4 2 5 6 7)
  (8 5 9 7 6 1 4 2 3)
  (4 2 6 8 5 3 7 9 1)
  (7 1 3 9 2 4 8 5 6)
  (9 6 1 5 3 7 2 8 4)
  (2 8 7 4 1 9 6 3 5)
  (3 4 5 2 8 6 1 7 9))

Core Functions
sudoku?
(sudoku? board) → boolean


Checks whether a completed board is a valid Sudoku solution.

All rows contain digits 1–9 exactly once

All columns contain digits 1–9 exactly once

All 3×3 subgrids contain digits 1–9 exactly once

solve-sudoku
(solve-sudoku board) → solved-board or false


Attempts to solve a Sudoku puzzle.

Returns a solved board if the puzzle is solvable

Returns false if the puzzle is invalid or unsolvable

Constraint Helpers

Single-cell detection

Row elimination

Column elimination

Subgrid elimination

Repeated propagation until no singles remain

Testing

Uses check-expect for all tests

Includes:

Valid solved boards

Invalid boards

Edge cases

Full-coverage solver tests

All solver tests use solvable puzzles only

Example test:

(check-expect
 (sudoku? '((5 3 4 6 7 8 9 1 2)
            (6 7 2 1 9 5 3 4 8)
            (1 9 8 3 4 2 5 6 7)
            (8 5 9 7 6 1 4 2 3)
            (4 2 6 8 5 3 7 9 1)
            (7 1 3 9 2 4 8 5 6)
            (9 6 1 5 3 7 2 8 4)
            (2 8 7 4 1 9 6 3 5)
            (3 4 5 2 8 6 1 7 9)))
 true)

Design Constraints

This project intentionally follows common academic constraints:

No mutation (set!)

No list-ref

No imperative loops

Heavy use of recursion and local

Clear helper decomposition

How to Run

Open the project in DrRacket

Run the file

All tests will execute automatically

Solver functions can be called from the REPL

File Structure
sudoku.rkt
README.md

Author

Zainulabdin Bughio

Notes

This project is intended for learning and demonstration purposes, focusing on functional design, correctness, and test coverage rather than performance optimizations.

