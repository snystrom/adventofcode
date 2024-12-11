#![allow(unused)]
use std::fs::read_to_string;

fn read_disk(filename: &str) -> Vec<i32> {
    let line = read_to_string(filename);

    line.expect("could not read")
        .trim()
        .to_string()
        .split("")
        .filter(|s| !s.is_empty()) //remove trailing
        .map(|s| s.parse::<i32>().expect("must be ints"))
        .collect()
}


//[1] "00...111...2...333.44.5555.6666.777.888899"
fn decompress(raw_disk: Vec<i32>) -> Vec<Option<i64>> {
    let mut v = Vec::<Option<i64>>::new();

    let mut file_idx : i64 = 0;
    for (i, value) in raw_disk.iter().enumerate() {
        //println!("at: {i}");

        if (i % 2 == 0) {
            for n in 0..(*value) {
                //println!("{i}");
                v.push(Some(file_idx));
            }
            file_idx += 1;
        } else { // free blocks
            for n in 0..(*value) {
                v.push(None);
            }
        }
    }
    v
}

fn main() {
    //let mut disk = ["0", ".", ".", "1", "1", "1", ".", ".", ".", ".", "2", "2", "2", "2", "2"];
    //let mut disk = ["0", "0", ".", ".", ".", "1", "1", "1", ".", ".", ".", "2", ".", ".", ".", "3", "3", "3", ".", "4", "4", ".", "5", "5", "5", "5", ".", "6", "6", "6", "6", ".", "7", "7", "7", ".", "8", "8", "8", "8", "9", "9"];

    //let dl = read_disk("example.txt");
    let dl = read_disk("input.txt");
    //println!("{dl:?}");
    //println!("decompress into:");
    let mut disk = decompress(dl);

    //println!("{disk:?}");
    let empty_loc : Vec<usize> = disk.iter()
        .enumerate()
        .filter(|(_, value)| value.is_none())
        .map(|(idx, _)| idx)
        .collect();

    let occ_loc : Vec<usize> = disk.iter()
        .enumerate()
        .filter(|(_, value)| value.is_some())
        .map(|(idx, _)| idx)
        .collect();

    //println!("{empty_loc:?}");
    //println!("{occ_loc:?}");

    for (i, (empty, occ)) in empty_loc.iter().zip(occ_loc.iter().rev()).enumerate() {
        //println!("{i} {empty:?} {occ:?}");
        if *empty > *occ {
            //println!("break");
            break
        }
        disk.swap(*empty,*occ);
    }
    //println!("{disk:?}");

    let part1 : i64 = disk.iter()
        .enumerate()
        .filter(|(_, val)| val.is_some())
        .map(|(idx, val)| idx as i64 * val.expect("must be an i64"))
        .sum();
    println!("Part1: {part1}")
}
