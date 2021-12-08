use std::{cmp::min, fs};

fn get_positions() -> Vec<u64> {
    let positions: Vec<u64> = fs::read_to_string("input")
        .expect("Unable to read input")
        .split(",")
        .filter(|s| !s.is_empty())
        .map(|s| {
            s.trim()
                .parse()
                .expect(format!("Invalid number '{}'", s).as_str())
        })
        .collect::<Vec<u64>>();

    positions
}

fn summation(n: u64) -> u64 {
    n * (n + 1) / 2
}

fn calculate_cost(target: u64, positions: &Vec<u64>) -> u64 {
    let mut cost = 0;

    for &x in positions {
        if x < target {
            cost += target - x;
        } else {
            cost += x - target;
        }
    }

    cost
}

fn calculate_cost_2(target: u64, positions: &Vec<u64>) -> u64 {
    let mut cost = 0;

    for &x in positions {
        if x < target {
            cost += summation(target - x);
        } else {
            cost += summation(x - target);
        }
    }

    cost
}

fn main() {
    let positions = get_positions();
    let mut minimum = u64::MAX;
    let mut minimum_2 = u64::MAX;

    for i in 0..*positions.iter().max().unwrap() {
        minimum = min(minimum, calculate_cost(i, &positions));
        minimum_2 = min(minimum_2, calculate_cost_2(i, &positions));
    }

    println!("Minimum: {}", minimum);
    println!("Minimum 2: {}", minimum_2);
}
