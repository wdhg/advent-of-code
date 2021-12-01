use std::fs;

fn main() {
    let depths: Vec<u32> = fs::read_to_string("input")
        .expect("Unable to read input")
        .split("\n")
        .filter(|s| !s.is_empty())
        .map(|s| s.parse().expect("Unable to parse depth"))
        .collect::<Vec<u32>>();

    let mut increases = 0;

    for i in 0..depths.len() - 1 {
        if depths[i] < depths[i + 1] {
            increases += 1;
        }
    }

    println!("Number of increases: {}", increases);

    increases = 0;

    for i in 0..depths.len() - 3 {
        let curr_sum = depths[i] + depths[i + 1] + depths[i + 2];
        let next_sum = depths[i + 1] + depths[i + 2] + depths[i + 3];

        if curr_sum < next_sum {
            increases += 1;
        }
    }

    println!("Number of increasing sums: {}", increases);
}
