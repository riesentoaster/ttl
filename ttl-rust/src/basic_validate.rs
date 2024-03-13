use std::{
    fmt::Display,
    ops::{Deref, DerefMut},
};

use crate::{
    board::{transpose, Board, BoardPrinter, Validator},
    field::Field,
};

#[derive(Clone)]
pub struct BasicValidator(Board);

impl BasicValidator {
    pub fn new(board: Board) -> Self {
        BasicValidator(board)
    }
}

impl Validator for BasicValidator {
    fn is_valid(&self) -> bool {
        let board: &Board = &self.0;
        if !board
            .iter()
            .all(|row| row_complies_with_field_count(row) && row_complies_with_triplets(row))
        {
            return false;
        }
        if !complies_with_unique_rows(board) {
            return false;
        }
        let transposed = transpose(board);
        if !transposed
            .iter()
            .all(|row| row_complies_with_field_count(row) && row_complies_with_triplets(row))
        {
            return false;
        }
        complies_with_unique_rows(&transposed)
    }
}

impl Display for BasicValidator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        BoardPrinter(self.deref()).fmt(f)
    }
}

impl Deref for BasicValidator {
    type Target = Board;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for BasicValidator {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

fn row_complies_with_field_count(row: &[Field]) -> bool {
    let mut x_count = 0;
    let mut o_count = 0;
    let max = row.len() / 2;
    for f in row {
        match f {
            Field::X => {
                x_count += 1;
                if x_count > max {
                    return false;
                }
            }
            Field::O => {
                o_count += 1;
                if o_count > max {
                    return false;
                }
            }
            _ => {}
        }
    }
    true
}

fn row_complies_with_triplets(row: &[Field]) -> bool {
    row.windows(3).all(|window| match window {
        [l, m, r] => *l == Field::E || *l != *m || *l != *r,
        _ => false,
    })
}

fn complies_with_unique_rows(board: &Board) -> bool {
    let rows_to_check = board.iter().filter(|row| !row.contains(&Field::E));
    for (row_i, row) in rows_to_check.clone().enumerate() {
        for (compare_row_i, compare_row) in rows_to_check.clone().enumerate() {
            if row_i < compare_row_i && row == compare_row {
                return false;
            }
        }
    }
    true
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{simple_validate::SimpleValidator, test_helpers::generate_random_board};
    use rayon::prelude::*;

    #[test]
    fn test_against_simple_validator() {
        (0..100000).into_par_iter().for_each(|_| {
            let board = generate_random_board(6, 8);
            let simple_validator_wrapper = SimpleValidator::new(board.clone());
            let basic_validator_wrapper = BasicValidator::new(board);
            assert_eq!(
                simple_validator_wrapper.is_valid(),
                basic_validator_wrapper.is_valid(),
                "------\n{}\n{}",
                simple_validator_wrapper,
                basic_validator_wrapper
            );
        })
    }
}
