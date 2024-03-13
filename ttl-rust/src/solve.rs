use std::ops::{Deref, DerefMut};

use crate::{
    board::{Board, Validator},
    field::Field::{E, O, X},
};

pub fn solve<T: Deref<Target = Board> + DerefMut + Validator + Clone>(board: &T) -> Vec<T> {
    let mut stack: Vec<T> = vec![board.clone()];
    let mut solutions: Vec<T> = Vec::new();
    while let Some(current) = stack.pop() {
        if !current.is_valid() {
            continue;
        }

        let inner = current.deref();

        if !inner.iter().flatten().any(|&field| field == E) {
            solutions.push(current.clone());
        }

        'outer: for i in 0..inner.len() {
            for j in 0..inner[0].len() {
                if inner[i][j] == E {
                    for value in [X, O] {
                        let mut c = current.clone();
                        c.deref_mut()[i][j] = value;
                        stack.push(c);
                    }
                    break 'outer;
                }
            }
        }
    }
    solutions
}
