use std::fs::read_to_string;

use std::collections::HashMap;

use tracing::{debug, error, info, span, Level};
use tracing_subscriber;

fn read_lines(filename: &str) -> (Vec<i32>, Vec<i32>) {
    let lines = read_to_string(filename);

    let mut col1 = Vec::new();
    let mut col2 = Vec::new();

    for line in lines.unwrap().lines() {
        debug!("line: {}", line);
        let parts: Vec<String> = line
            .trim()
            .to_string()
            .split_whitespace()
            .map(|x| x.to_string())
            .collect();

        debug!("parts: {:?}", parts);
        debug!("v1: {:?}, v2: {:?}", parts[0], parts[1]);
        let v1: i32 = parts[0].parse().unwrap();
        let v2: i32 = parts[1].parse().unwrap();

        col1.push(v1);
        col2.push(v2);
    }
    (col1, col2)
}

fn part1(file: &str) -> i32 {
    let (mut col1, mut col2) = read_lines(file);

    col1.sort();
    col2.sort();

    let answer: i32 = col1
        .iter()
        .zip(col2.iter())
        .map(|(x, y)| (x - y).abs())
        .collect::<Vec<i32>>()
        .into_iter()
        .sum();
    answer
}

fn part2(file: &str) -> i32 {
    let (col1, col2) = read_lines(file);
    let mut number_counts: HashMap<i32, i32> = HashMap::new();

    // Count number appearance in col2
    for value in col2 {
        number_counts
            .entry(value)
            .and_modify(|count| *count += 1)
            .or_insert(1);
    }

    let mut similarity_score: i32 = 0;

    for val in col1 {
        similarity_score += val * number_counts.get(&val).unwrap_or(&0);
    }
    similarity_score
}

fn main() {
    tracing_subscriber::fmt().with_max_level(Level::INFO).init();

    span!(Level::DEBUG, "start");

    //let file = "example.txt";
    let file = "input.txt";
    let part1 = part1(file);

    println!("{part1}");

    //let part2_input = "example.txt";
    let part2_input = "input.txt";
    let part2 = part2(part2_input);

    println!("{part2}");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        let ans = part1("example.txt");
        assert_eq!(ans, 11);
    }

    #[test]
    fn test_part2() {
        let ans = part2("example.txt");
        assert_eq!(ans, 31);
    }
}
