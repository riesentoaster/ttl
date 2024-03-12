use std::fmt::Display;

use crate::{
    board::{transpose, Board, BoardPrinter, InnerValue, Validator},
    field::Field::{E, O, X},
};

#[derive(Clone)]
pub struct SimpleValidator(Board);

impl SimpleValidator {
    pub fn new(board: Board) -> Self {
        SimpleValidator(board)
    }
}

impl InnerValue<Board> for SimpleValidator {
    fn inner_value_mut(&mut self) -> &mut Board {
        &mut self.0
    }

    fn inner_value(&self) -> &Board {
        &self.0
    }
}

impl Display for SimpleValidator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        BoardPrinter(self.inner_value()).fmt(f)
    }
}

impl Validator for SimpleValidator {
    fn is_valid(&self) -> bool {
        let board: &Board = &self.0;
        let transposed = &transpose(board);
        rows_comply_with_triplets(board)
            && rows_comply_with_triplets(transposed)
            && rows_comply_with_max_count(board)
            && rows_comply_with_max_count(transposed)
            && rows_comply_with_uniqueness(board)
            && rows_comply_with_uniqueness(transposed)
    }
}

fn rows_comply_with_triplets(board: &Board) -> bool {
    board.iter().all(|row| {
        row.windows(3).all(|w| match w {
            [l, m, r] => *l == E || *l != *m || *l != *r,
            _ => false,
        })
    })
}

fn rows_comply_with_max_count(board: &Board) -> bool {
    board.iter().all(|row| {
        let max_length = row.len() / 2;
        let xs = row.iter().filter(|e| **e == X).count();
        let os = row.iter().filter(|e| **e == O).count();
        xs <= max_length && os <= max_length
    })
}
fn rows_comply_with_uniqueness(board: &Board) -> bool {
    for (i, r) in board.iter().enumerate() {
        for (j, s) in board.iter().enumerate() {
            if i != j && !r.iter().any(|e| *e == E) && r == s {
                return false;
            }
        }
    }
    true
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_helpers::parse_board;

    #[test]
    fn test_triplets() {
        let boards = [
            ("EEEE|EEEE|EEEE|EEEE", true, "no Xs"),
            ("XEEE|EEEE|EEEE|EEEE", true, "single element"),
            ("XXEE|XEEE|EEEE|EEEE", true, "two in a row, empty around"),
            ("XXOE|XOEE|OEEE|EEEE", true, "two in a row, other around"),
            ("XXXE|XEEE|EEEE|EEEE", false, "three in a row horizontally"),
            ("XXEE|XEEE|XEEE|EEEE", false, "three in a row vertically"),
            ("XXXX|XEEE|EEEE|EEEE", false, "four in a row horizontally"),
            ("XXEE|XEEE|XEEE|XEEE", false, "four in a row vertically"),
            ("XOXE|XEEE|EEEE|EEEE", true, "three in a row h interspersed"),
            ("XXEE|OEEE|XEEE|EEEE", true, "three in a row v interspersed"),
        ];
        for (board, expected, message) in boards {
            let board = parse_board(board).unwrap();
            let wrapper = SimpleValidator::new(board);
            let is_valid = wrapper.is_valid();
            assert_eq!(
                is_valid, expected,
                "{} — expected {} for board {}",
                message, expected, wrapper
            );
        }
    }
}
