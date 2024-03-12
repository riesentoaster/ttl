use std::fmt::{Display, Formatter, Result};

use crate::field::Field;

pub type Board = Vec<Vec<Field>>;

pub fn transpose<T: Copy>(board: &[Vec<T>]) -> Vec<Vec<T>> {
    (0..board[0].len())
        .map(|i| board.iter().map(|row| row[i]).collect())
        .collect()
}

pub trait InnerValue<T> {
    fn inner_value_mut(&mut self) -> &mut T;
    fn inner_value(&self) -> &T;
}

pub trait Validator {
    fn is_valid(&self) -> bool;
}

pub struct BoardPrinter<'a>(pub &'a Board);

impl<'a> Display for BoardPrinter<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        self.0.iter().try_for_each(|row| {
            row.iter().try_for_each(|field| write!(f, "{}", field))?;
            writeln!(f)
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_helpers::parse_board;

    #[test]
    fn test_transpose() {
        let grid = vec![vec![1, 2, 3, 4], vec![5, 6, 7, 8], vec![9, 10, 11, 12]];
        let transposed = vec![
            vec![1, 5, 9],
            vec![2, 6, 10],
            vec![3, 7, 11],
            vec![4, 8, 12],
        ];
        assert_eq!(transpose(&grid), transposed)
    }

    #[test]
    fn test_print() {
        let str = "X..O|O.X.";
        let board = parse_board(str).unwrap();
        let mut expect = str.replace('|', "\n");
        expect.push('\n');
        let actual = format!("{}", BoardPrinter(&board));
        assert_eq!(actual, expect, "Actual:\n{}\nExpected:\n{}", actual, expect);
    }
}
