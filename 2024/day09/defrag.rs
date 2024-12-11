#![allow(unused)]
use std::fs::read_to_string;
use std::collections::{BTreeMap, VecDeque};

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

fn part1() {
    //let mut disk = ["0", ".", ".", "1", "1", "1", ".", ".", ".", ".", "2", "2", "2", "2", "2"];
    //let mut disk = ["0", "0", ".", ".", ".", "1", "1", "1", ".", ".", ".", "2", ".", ".", ".", "3", "3", "3", ".", "4", "4", ".", "5", "5", "5", "5", ".", "6", "6", "6", "6", ".", "7", "7", "7", ".", "8", "8", "8", "8", "9", "9"];

    //let dl = read_disk("example.txt");
    let dl = read_disk("input.txt");

    //println!("{dl:?}");
    //println!("decompress into:");
    let mut disk = decompress(dl);

    println!("{disk:?}");
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
    println!("{disk:?}");

    let part1 : i64 = disk.iter()
        .enumerate()
        .filter(|(_, val)| val.is_some())
        .map(|(idx, val)| idx as i64 * val.expect("must be an i64"))
        .sum();
    println!("Part1: {part1}")
}

fn get_empty_block_map(disk: &Vec<Option<i64>>) -> BTreeMap<usize, VecDeque<usize>> {
    let empty_loc : Vec<usize> = disk.iter()
        .enumerate()
        .filter(|(_, value)| value.is_none())
        .map(|(idx, _)| idx)
        .collect();

    // This will be a map of indices for each block
    // so for a disk: [00..11.2]
    // {0: [2,3], 1: [6]}
    // values are a dequeue so we can pop from the front as we fill
    let mut empty_disk_blocks = BTreeMap::new();

    let mut block_id = 0;
    let mut prev = None;
    for cur in empty_loc.iter() {
        if let Some(p) = prev {
            if *cur == p + 1 {
                empty_disk_blocks.entry(block_id)
                    .or_insert_with(VecDeque::new)
                    .push_back(cur.to_owned())
            } else {
                block_id += 1;
                empty_disk_blocks.entry(block_id)
                        .or_insert_with(VecDeque::new)
                        .push_back(cur.to_owned())
            }
        } else {
            empty_disk_blocks.entry(block_id)
                    .or_insert_with(VecDeque::new)
                    .push_back(cur.to_owned())
        }

        prev = Some(cur);
    }

    empty_disk_blocks
}

fn get_file_blocks(disk: &Vec<Option<i64>>) -> BTreeMap::<usize, Vec<usize>> {
    let occ_loc : Vec<usize> = disk.iter()
        .enumerate()
        .filter(|(_, value)| value.is_some())
        .map(|(idx, _)| idx)
        .collect();

    // hold the spots to insert each file block
    let mut file_blocks = BTreeMap::new();

    let mut block_id = 0;
    let mut prev = None;
    for cur in occ_loc.iter() {
        let cur_file = unsafe { *disk.get_unchecked(*cur) };

        // If we're at start, initialize at block 0
        if prev.is_none() {
            file_blocks.entry(block_id)
                .or_insert_with(Vec::new)
                .push(cur.to_owned())
        }

        if let Some(p) = prev {
            // SAFETY: these are idx's so must be inbounds
            if cur_file == p {
                file_blocks.entry(block_id)
                    .or_insert_with(Vec::new)
                    .push(cur.to_owned())
            } else {
                block_id += 1;
                file_blocks.entry(block_id)
                    .or_insert_with(Vec::new)
                    .push(cur.to_owned())
            }
            prev = Some(cur_file);
        } else {
            prev = Some(cur_file);
        }
    }

    file_blocks
}

fn part2() {
    //let mut disk = decompress(read_disk("input.txt"));
    let mut disk = decompress(read_disk("example.txt"));
    //println!("{disk:?}");


    let mut empty_disk_blocks = get_empty_block_map(&disk);
    println!("{empty_disk_blocks:?}");


    let mut file_blocks = get_file_blocks(&disk);
    println!("{file_blocks:?}");

    for (file_id, file) in file_blocks.iter_mut().rev() {
        println!("file_id: {file_id}");
        let mut has_moved = false;
        for (empty_id, empty) in empty_disk_blocks.iter_mut() {
            println!("empty_id: {empty_id}");
            println!("{empty:?}");
            if empty.len() >= file.len() {

                // TODO: while let Some(i) = file.pop()
                for i in file.iter() {
                    let j = empty.pop_front().expect("Empty block has capacity because of length check");
                    if (*i < j) {
                        break
                    }
                    println!("file_i: {i}, empty_j: {j}");
                    disk.swap(*i,j);
                }
                has_moved = true;
            }
            if (has_moved) {
                break
            }
        }
    }

    println!("{disk:?}");

    let x = disk.iter()
        .map(|x| match x {
            Some(value) => value.to_string(),
            None => ".".to_string()
        })
        .collect::<Vec<_>>()
        .join("");

    println!("{x}");







    // for each file, check whether
    //for loc in occ_loc {

    //}

    //let empty_blocks = disk.split_mut(|x| x.is_some());

    //for empty in empty_blocks {
    //    empty.len()

    //}


    /*
    for block in file_blocks {
        // TODO: get file boundaries in a block
        let cur_file_id = block.last().expect("can't get last");

        let cur_file_len = block.iter()
            .rev()
            .take_while(|x| *x == cur_file_id)
            .count();

        let cur_file_idx_iter = block.iter()
            .rev()
            .enumerate()
            .take_while(|(_,value)| *value == cur_file_id)
            .map(|(idx, _)| idx);


        let empty_blocks = disk.split_mut(|x| x.is_some());

        for empty in empty_blocks {
            if empty.len() >= cur_file_len {
                let mut ei = 0;
                for i in cur_file_idx_iter {
                    empty[ei] = block[i];
                    block[i] = None;
                }
            }
        }
    }

    println!("{disk:?}");
     */
}

fn main() {
    part1();
    part2();
}
