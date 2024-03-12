use crate::{
    board::{Board, InnerValue, Validator},
    field::Field::{E, O, X},
};

pub fn solve<T: InnerValue<Board> + Validator + Clone>(board: &T) -> Vec<T> {
    let mut stack: Vec<T> = vec![board.clone()];
    let mut solutions: Vec<T> = Vec::new();
    while let Some(current) = stack.pop() {
        if !current.is_valid() {
            continue;
        }

        let inner = current.inner_value();

        if !inner.iter().flatten().any(|&field| field == E) {
            solutions.push(current.clone());
        }

        'outer: for i in 0..inner.len() {
            for j in 0..inner[0].len() {
                if inner[i][j] == E {
                    for value in [X, O] {
                        let mut c = current.clone();
                        c.inner_value_mut()[i][j] = value;
                        stack.push(c);
                    }
                    break 'outer;
                }
            }
        }
    }
    solutions
}
