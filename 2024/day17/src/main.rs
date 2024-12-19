use std::io;
use std::io::Read;

fn output(vec: &Vec<u8>) -> String {
    vec.iter().map(|c| c.to_string()).collect::<Vec<String>>().join(",")
}

fn main() -> io::Result<()> {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input)?;

    let mut registers = [0; 3];
    let mut instructions = Vec::new();

    for line in input.lines() {
        if line.starts_with("Register A: ") {
            registers[0] = line[12..].parse().unwrap();
        }
        if line.starts_with("Register B: ") {
            registers[1] = line[12..].parse().unwrap();
        }
        if line.starts_with("Register C: ") {
            registers[2] = line[12..].parse().unwrap();
        }

        if line.starts_with("Program: ") {
            for char in line[9..].split(',') {
                instructions.push(char.as_bytes()[0]-'0' as u8);
            }
        }
    }

    let mut new_program = run(&instructions, registers.clone());
    println!("Part 1: {}", output(&new_program));

    let mut a_register = 1;
    loop {
        loop {
            registers[0] = a_register;
            new_program = run(&instructions, registers.clone());

            if instructions.ends_with(&new_program) { break; }
            a_register += 1;
        }
        if new_program.eq(&instructions) { break; }
        a_register <<= 3;
    }

    println!("Part 2: {}", a_register);

    Ok(())
}

const ADV: u8 = 0;
const BXL: u8 = 1;
const BST: u8 = 2;
const JNZ: u8 = 3;
const BXC: u8 = 4;
const OUT: u8 = 5;
const BDV: u8 = 6;
const CDV: u8 = 7;

fn run(instructions: &Vec<u8>, mut registers: [i64; 3]) -> Vec<u8> {
    let mut ip = 0;

    let mut stdout = Vec::new();

    loop {
        if ip+1 >= instructions.len() { break; }

        let ins = instructions[ip];
        let op = instructions[ip+1];

        match ins {
            ADV => registers[0] /= 2i64.pow(combo(&registers, op) as u32),
            BXL => registers[1] ^= op as i64,
            BST => registers[1] = combo(&registers, op) % 8,
            JNZ => if registers[0] != 0 { ip = op as usize; continue; }
            BXC => registers[1] ^= registers[2],
            OUT => stdout.push(combo(&registers, op) as u8 % 8),
            BDV => registers[1] = registers[0] / 2i64.pow(combo(&registers, op) as u32),
            CDV => registers[2] = registers[0] / 2i64.pow(combo(&registers, op) as u32),
            _ => panic!("Unknown instruction: {}", ins),
        }
        ip += 2;
    }

    stdout
}

fn combo(registers: &[i64; 3], op: u8) -> i64 {
    match op {
        0..=3 => op as i64,
        4 => registers[0],
        5 => registers[1],
        6 => registers[2],
        7 => panic!("Using reserved combo-op 7!"),
        _ => panic!("Using unknown combo-op: {}", op),
    }
}