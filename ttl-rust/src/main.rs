use solve::solve;

use crate::{
    basic_validate::BasicValidator, board::Board, field::Field, simple_validate::SimpleValidator,
};

pub mod basic_validate;
pub mod board;
pub mod field;
pub mod simple_validate;
pub mod solve;
pub mod test_helpers;

const WIDTH: usize = 6;
const HEIGHT: usize = 6;

fn main() {
    let board: Board = vec![vec![Field::E; WIDTH]; HEIGHT];
    let wrapped = SimpleValidator::new(board);
    let solutions = solve(&wrapped);
    for solution in solutions {
        println!("{}", solution);
    }
}
