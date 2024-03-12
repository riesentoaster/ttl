use crate::{
    board::Board,
    field::Field::{E, O, X},
};

#[allow(dead_code)]
pub fn parse_board(str: &str) -> Option<Board> {
    str.split('|')
        .map(|r| {
            r.chars()
                .map(|e| match e {
                    'E' => Some(E),
                    '.' => Some(E),
                    'X' => Some(X),
                    'O' => Some(O),
                    _ => None,
                })
                .collect()
        })
        .collect()
}

pub fn generate_random_board(height: usize, width: usize) -> Board {
    (0..height)
        .map(|_| (0..width).map(|_| rand::random()).collect())
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::field::Field::{O, X};

    #[test]
    fn test_parse() {
        let board = parse_board(".O|EX");
        assert!(board.is_some());
        assert_eq!(vec![vec![E, O], vec![E, X]], board.unwrap());
    }
}
