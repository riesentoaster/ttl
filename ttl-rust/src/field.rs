use std::fmt::{Display, Formatter, Result};

use rand::{
    distributions::{Distribution, Standard},
    Rng,
};

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Field {
    X,
    O,
    E,
}

impl Display for Field {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Field::X => write!(f, "X"),
            Field::O => write!(f, "O"),
            Field::E => write!(f, "."),
        }
    }
}

impl Distribution<Field> for Standard {
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> Field {
        // match rng.gen_range(0, 3) { // rand 0.5, 0.6, 0.7
        match rng.gen_range(0..=2) {
            // rand 0.8
            0 => Field::E,
            1 => Field::X,
            _ => Field::O,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Field::{E, O, X};

    #[test]
    fn test_random() {
        let count: i64 = 1000000;
        let res =
            (0..count)
                .map(|_| rand::random())
                .fold((0, 0, 0), |(es, xs, os), cur| match cur {
                    E => (es + 1, xs, os),
                    X => (es, xs + 1, os),
                    O => (es, xs, os + 1),
                });
        assert!(res.0 > count / 4);
        assert!(res.1 > count / 4);
        assert!(res.2 > count / 4);
    }
}
