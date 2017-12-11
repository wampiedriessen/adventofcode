use std::fmt::Write;

fn hash_step(v: &mut [u8; 256], index:&mut u8, skipsize:&mut u8, len: u8) {
    let start = index.clone();
    for i in 0..(len / 2) {
        let a = start.wrapping_add(i);
        let b = start.wrapping_add(len).wrapping_sub(i).wrapping_sub(1);
        let tmp = v[a as usize];
        v[a as usize] = v[b as usize];
        v[b as usize] = tmp;
    }

    *index = index.wrapping_add(len.clone()).wrapping_add(*skipsize);
    *skipsize = skipsize.wrapping_add(1);
}

fn dense_hash(v: &[u8;256]) -> [u8; 16] {
    let mut hash:[u8; 16] = [0; 16];

    for i in 0..16 {
        let mut val:u8 = v[i*16];
        for j in 1..16 {
            val ^= v[i*16 + j];
        }
        hash[i] = val;
    }

    return hash;
}

fn main() {
    let input = "63,144,180,149,1,255,167,84,125,65,188,0,2,254,229,24".to_string();
    let mut list: [u8; 256] = [0; 256];

    for i in 0..256 {
        list[i] = i as u8;
    }

    print!("Part 1: ");
    part1(input.clone(), list.clone());
    print!("Part 2: ");
    part2(input, list);
}

fn part1(input: String, mut list: [u8; 256]) {
    let mut index:u8 = 0;
    let mut skipsize:u8 = 0;

    for length in input.split(",") {
        let len = length.parse::<u8>().unwrap();

        hash_step(&mut list, &mut index, &mut skipsize, len);
    }

    println!("{:?},", list[0] as u32 * list[1] as u32);
}

fn part2(input: String, mut list: [u8; 256]) {
    let mut index:u8 = 0;
    let mut skipsize:u8 = 0;

    let mut bytes:Vec<u8> = input.into_bytes();
    bytes.push(17);
    bytes.push(31);
    bytes.push(73);
    bytes.push(47);
    bytes.push(23);

    for _i in 0..64 {
        for len in &bytes {
            hash_step(&mut list, &mut index, &mut skipsize, *len);
        }
    }

    let dh = dense_hash(&list);
    let mut hash: String = String::new();
    for &byte in dh.iter() {
        write!(&mut hash, "{:02x}", byte).unwrap();
    }
    println!("{:?}", hash);
}