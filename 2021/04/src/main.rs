use std::fs;
use Option::*;

const BOARD_SIZE: usize = 5;

#[derive(Clone, Copy)]
struct Board {
    numbers: [Option<u8>; BOARD_SIZE * BOARD_SIZE],
    row_counts: [u8; BOARD_SIZE],
    col_counts: [u8; BOARD_SIZE],
}

impl Board {
    fn from_string(string: &str) -> Board {
        let mut numbers = [None; BOARD_SIZE * BOARD_SIZE];

        numbers.copy_from_slice(
            string
                .split_whitespace()
                .map(|s| {
                    Some(
                        s.parse::<u8>()
                            .expect(format!("Unable to parse '{}'", s).as_str()),
                    )
                })
                .collect::<Vec<Option<u8>>>()
                .as_slice(),
        );

        Board {
            numbers,
            row_counts: [0; BOARD_SIZE],
            col_counts: [0; BOARD_SIZE],
        }
    }

    fn mark(&mut self, number: u8) {
        for i in 0..self.numbers.len() {
            match self.numbers[i] {
                Some(n) if number == n => self.mark_index(i),
                _ => continue,
            }
        }
    }

    fn mark_index(&mut self, index: usize) {
        let row = index / BOARD_SIZE;
        let col = index % BOARD_SIZE;

        self.row_counts[row] += 1;
        self.col_counts[col] += 1;
        self.numbers[index] = None;
    }

    fn has_won(&self) -> bool {
        for count in [self.row_counts, self.col_counts].concat() {
            if count == BOARD_SIZE as u8 {
                return true;
            }
        }

        false
    }

    fn calculate_score(&self, number: u8) -> u32 {
        self.numbers
            .iter()
            .map(|x| x.unwrap_or(0) as u32)
            .sum::<u32>()
            * (number as u32)
    }
}

fn read_input() -> (Vec<u8>, Vec<Board>) {
    let file_contents = fs::read_to_string("input").expect("Unable to read input");
    let strings = file_contents.split("\n\n").collect::<Vec<&str>>();
    let (random_numbers_string, board_strings) = strings
        .as_slice()
        .split_first()
        .expect("Failed to parse input");
    (
        random_numbers_string
            .split(",")
            .map(|s| {
                s.parse::<u8>()
                    .expect(format!("Unable to parse '{}'", s).as_str())
            })
            .collect(),
        board_strings
            .iter()
            .map(|s| Board::from_string(*s))
            .collect(),
    )
}

fn main() {
    let (random_numbers, mut boards) = read_input();

    for random_number in random_numbers {
        for i in 0..boards.len() {
            if boards[i].has_won() {
                continue;
            }

            boards[i].mark(random_number);

            if boards[i].has_won() {
                println!(
                    "Board {} score: {}",
                    i,
                    boards[i].calculate_score(random_number)
                );
            }
        }
    }
}
