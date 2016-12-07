extern crate crypto;

use std::io;

use crypto::md5::Md5;
use crypto::digest::Digest;

fn main() {
    
    let stdin = io::stdin();

    let mut buffer = String::new();

    stdin.read_line(&mut buffer).expect("no line read?");

    println!("{:?}", buffer.trim());

    let mut count: usize = 0;
    let mut word: Vec<u8> = Vec::with_capacity(8);
    let mut used_index: Vec<bool> = Vec::new();
    used_index.resize(8, false);
    unsafe {
        word.set_len(8);
    }
    let key = buffer.trim().as_bytes();

    let mut hasher = Md5::new();
    'outer: for j in 0..std::u64::MAX {
        hasher.input(key);
        hasher.input(j.to_string().as_bytes());

        let mut output = [0; 16];
        hasher.result(&mut output);

        hasher.reset();

        if output[0] != 0 {
            continue 'outer;
        }
        if output[1] != 0 {
            continue 'outer;
        }
        if (output[2] >> 4) != 0 {
            continue 'outer;
        }
        // word.push(output[2] & 0xf);
        count += 1;

        print!("j: {}, output: -", j);
        print!("{}-", output[0]);
        print!("{}-", output[1]);
        print!("{}-", (output[2] >> 4));

        let index: u8 = output[2] & 0xf;

        print!("charv1: {}", index);

        if index >= 8 || used_index[index as usize] {
            count -= 1;
            print!("\n");
            continue;
        }

        word[index as usize] = output[3] >> 4;
        used_index[index as usize] = true;

        print!("\n");

        if count == 8 {
            break;
        }
    }

    for x in word {
        print!("{}-", x);
    }
    print!("\n");

}