# TTL

A collection of solvers for a simple sudoku-like game.

## Rules

- The board consists of a two-dimensional grid with an even number of rows and columns.
- Each field can be either empty, or filled with an `X` or an `O`.
- The goal of the player is to fill the board with `X`s and `O`s according to the following:

For all rows and for all columns (separately):

- No two may be exactly equal.
- It may have at most half the fields covered by `X`s and half with `O`s
- No three `O`s may appear consecutively, no three `X`s may appear consecutively.