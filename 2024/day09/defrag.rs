#![allow(unused)]
fn main() {
    //let mut disk = ["0", ".", ".", "1", "1", "1", ".", ".", ".", ".", "2", "2", "2", "2", "2"];
    let mut disk = ["0", "0", ".", ".", ".", "1", "1", "1", ".", ".", ".", "2", ".", ".", ".", "3", "3", "3", ".", "4", "4", ".", "5", "5", "5", "5", ".", "6", "6", "6", "6", ".", "7", "7", "7", ".", "8", "8", "8", "8", "9", "9"];


    println!("{disk:?}");
    let empty_loc : Vec<usize> = disk.iter()
        .enumerate()
        .filter(|(_, value)| value == &&".")
        .map(|(idx, _)| idx)
        .collect();

    let occ_loc : Vec<usize> = disk.iter()
        .enumerate()
        .filter(|(_, value)| value != &&".")
        .map(|(idx, _)| idx)
        .collect();

    println!("{empty_loc:?}");
    println!("{occ_loc:?}");

    for (i, (empty, occ)) in empty_loc.iter().zip(occ_loc.iter().rev()).enumerate() {
        println!("{i} {empty:?} {occ:?}");
        if *empty > *occ {
            println!("break");
            break
        }
        disk.swap(*empty,*occ);
    }
    println!("{disk:?}");

    let part1 : i64 = disk.iter()
        .enumerate()
        .filter(|(_, val)| val != &&".")
        .map(|(idx, val)| idx as i64 * val.parse::<i64>().expect("must be an int"))
        .sum();
    println!("Part1: {part1}")
}
