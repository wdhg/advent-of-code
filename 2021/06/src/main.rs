use std::fs;

fn simulate_day(fish_counts: [u64; 9]) -> [u64; 9] {
    let mut new_fish_counts = [0; 9];

    for i in 0..fish_counts.len() - 1 {
        new_fish_counts[i] += fish_counts[i + 1];
    }

    new_fish_counts[6] += fish_counts[0]; // reset timers
    new_fish_counts[8] += fish_counts[0]; // breed new fish

    new_fish_counts
}

fn get_fish_counts() -> [u64; 9] {
    let days: Vec<u8> = fs::read_to_string("input")
        .expect("Unable to read input")
        .split(",")
        .map(|s| {
            s.trim()
                .parse()
                .expect(format!("Unable to parse number '{}'", s).as_str())
        })
        .collect::<Vec<u8>>();

    let mut fish_counts = [0; 9];

    for day in days {
        fish_counts[day as usize] += 1;
    }

    fish_counts
}

fn main() {
    let mut fish_counts = get_fish_counts();
    println!("{:?}", fish_counts);

    for _ in 0..256 {
        fish_counts = simulate_day(fish_counts);
    }

    println!("{:?}", fish_counts);
    println!("Total {}", fish_counts.iter().sum::<u64>());
}
