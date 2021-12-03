use std::fs;

const BITS: usize = 12;
const MASK: u32 = (1 << BITS) - 1;

enum Preference {
    MostCommon,
    LeastCommon,
}

impl Preference {
    fn pick(&self, ones: Vec<u32>, zeros: Vec<u32>) -> Vec<u32> {
        let should_pick_ones = match self {
            Preference::MostCommon => ones.len() >= zeros.len(),
            Preference::LeastCommon => ones.len() < zeros.len(),
        };

        if should_pick_ones {
            ones
        } else {
            zeros
        }
    }
}

fn bit(x: u32, i: usize) -> u32 {
    (x >> i) & 1
}

fn test_bit(x: u32, i: usize) -> bool {
    bit(x, i) == 1
}

fn set_bit(x: u32, i: usize) -> u32 {
    x | (1 << i)
}

fn count_bit(numbers: &Vec<u32>, index: usize) -> u32 {
    numbers.iter().map(|x| bit(*x, index)).sum()
}

fn count_bits(numbers: &Vec<u32>) -> [u32; BITS] {
    let mut counts: [u32; BITS] = [0; BITS];

    for i in 0..BITS {
        counts[i] = count_bit(&numbers, i);
    }

    counts
}

fn find_gamma_rate(numbers: &Vec<u32>) -> u32 {
    let counts = count_bits(numbers);
    let threshold = numbers.len() as u32 / 2;
    let mut gamma_rate = 0;

    for i in 0..counts.len() {
        if counts[i] > threshold {
            gamma_rate = set_bit(gamma_rate, i);
        }
    }

    gamma_rate
}

fn find_rating(numbers: &Vec<u32>, preference: &Preference) -> u32 {
    let mut numbers = numbers.clone();

    for i in (0..BITS).rev() {
        let (ones, zeros): (Vec<u32>, Vec<u32>) = numbers.iter().partition(|x| test_bit(**x, i));
        numbers = preference.pick(ones, zeros);

        if numbers.len() == 1 {
            return numbers[0];
        }
    }
    unreachable!();
}

fn main() {
    let numbers: Vec<u32> = fs::read_to_string("input")
        .expect("Unable to read input")
        .split("\n")
        .filter(|s| !s.is_empty())
        .map(|s| u32::from_str_radix(s, 2).unwrap())
        .collect();

    let gamma_rate = find_gamma_rate(&numbers);
    let epsilon_rate = !gamma_rate & MASK;
    let oxygen_rating = find_rating(&numbers, &Preference::MostCommon);
    let carbon_rating = find_rating(&numbers, &Preference::LeastCommon);

    println!("            Gamma: {}", gamma_rate);
    println!("          Epsilon: {}", epsilon_rate);
    println!("Power Consumption: {}", gamma_rate * epsilon_rate);
    println!("           Oxygen: {}", oxygen_rating);
    println!("           Carbon: {}", carbon_rating);
    println!("     Life Support: {}", oxygen_rating * carbon_rating);
}
