use std::collections::HashMap;
use std::io;
use std::io::Read;

fn reorder_files_1(disk: &Vec<i64>) -> Vec<i64> {
    let mut disk = disk.clone();

    let mut c1 = 0;
    let mut c2 = disk.len() - 1;

    while c1 < c2 {
        if disk[c1] != -1 { c1 += 1; continue; }
        if disk[c2] == -1 { c2 -= 1; continue; }

        disk[c1] = disk[c2];
        disk[c2] = -1;
    }

    disk
}

fn reorder_files_2(disk: &Vec<i64>, filelengths: &HashMap<i64, i64>) -> Vec<i64> {
    let mut disk = disk.clone();

    let mut c2 = disk.len() - 1;

    let mut lastseens = [0usize; 10];
    while c2 > 0 {
        if disk[c2] == -1 { c2 -= 1; continue; }

        let filenum = disk[c2];

        let len = filelengths[&filenum] as usize;
        if c2 <= len { break;}
        c2 -= len;

        let lastseen = lastseens[len];
        let mut gap = usize::MAX;
        for (gapindex, win) in disk[lastseen..].windows(len).enumerate() {
            if win.iter().all(|x| *x == -1) {
                gap = gapindex + lastseen;
                break;
            }
        }

        if gap == usize::MAX { continue; }
        for i in len..10 {
            lastseens[i] = gap;
        }

        if gap >= c2 { continue;}

        for i in gap..gap+len {
            disk[i] = filenum;
        }

        for i in c2+1..c2+len+1 {
            disk[i] = -1;
        }
    }

    disk
}


fn checksum(disk: &Vec<i64>) -> i64 {
    disk.iter().enumerate().fold(0, |acc, (i, val)| if *val == -1 { acc } else { acc + i as i64 * *val })
}

fn main() -> io::Result<()> {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input)?;

    let numbers = input
        .trim()
        .chars()
        .into_iter()
        .map(|c| c.to_digit(10).unwrap())
        .collect::<Vec<u32>>();

    let mut disk = Vec::new();
    let mut filelengths = HashMap::new();

    let mut fileno: i64 = 0;
    for ch in numbers.chunks(2) {
        filelengths.insert(fileno, ch[0] as i64);
        for _ in 0..ch[0] {
            disk.push(fileno);
        }

        fileno += 1;
        // input might not be strictly divisible by 2
        if ch.len() > 1 {
            for _ in 0..ch[1] {
                disk.push(-1);
            }
        }
    }

    let disk_p1 = reorder_files_1(&disk);
    let disk_p2 = reorder_files_2(&disk, &filelengths);

    println!("Part 1: {}", checksum(&disk_p1));
    println!("Part 2: {}", checksum(&disk_p2));

    Ok(())
}
