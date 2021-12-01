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
        if depths[i] < depths[i + 3] {
            increases += 1;
        }
    }

    println!("Number of increasing sums: {}", increases);
}
