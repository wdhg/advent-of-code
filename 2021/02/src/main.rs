use std::fs;

#[derive(Debug, Clone, Copy)]
enum Instruction {
    Forward(u64),
    Up(u64),
    Down(u64),
}

impl Instruction {
    fn parse(s: &str) -> Self {
        use Instruction::*;

        let parts: Vec<&str> = s.split(" ").collect();
        let amount: u64 = parts[1].parse().expect("Unable to parse amount");

        match parts[0] {
            "forward" => Forward(amount),
            "up" => Up(amount),
            "down" => Down(amount),
            _ => panic!("Unable to parse Instruction {}", parts[0]),
        }
    }
}

fn main() {
    use Instruction::*;

    let instructions: Vec<Instruction> = fs::read_to_string("input")
        .expect("Unable to read input")
        .split("\n")
        .filter(|s| !s.is_empty())
        .map(Instruction::parse)
        .collect::<Vec<Instruction>>();

    let mut horizontal = 0;
    let mut depth = 0;

    for instruction in instructions.clone() {
        match instruction {
            Forward(v) => horizontal += v,
            Up(v) => depth -= v,
            Down(v) => depth += v,
        }
    }

    println!("Part 1: {}", horizontal * depth);

    let mut horizontal = 0;
    let mut depth = 0;
    let mut aim = 0;

    for instruction in instructions.clone() {
        match instruction {
            Forward(v) => {
                horizontal += v;
                depth += aim * v;
            }
            Up(v) => aim -= v,
            Down(v) => aim += v,
        }
    }

    println!("Part 2: {}", horizontal * depth);
}
