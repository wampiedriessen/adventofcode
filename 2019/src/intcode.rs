use std::collections::VecDeque;

pub type IntcodeProg = Vec<i64>;

const FINISHED: u8 = 0;
const WAITING_FOR_INPUT: u8 = 1;
const RUNNING: u8 = 2;

pub struct Intcode {
    pc: usize,
    ops: IntcodeProg,
    read_queue: VecDeque<i64>,
    write_queue: VecDeque<i64>,
    state: u8,
}

impl Intcode {
    pub fn new(program: &IntcodeProg) -> Intcode {
        Intcode {
            pc: 0,
            ops: program.clone(),
            read_queue: VecDeque::new(),
            write_queue: VecDeque::new(),
            state: RUNNING,
        }
    }

    pub fn read_input(input:&str) -> IntcodeProg {
        return input.split(",").map(|x| x.parse().unwrap()).collect();
    }

    pub fn compute(&mut self) -> () {
        self.state = RUNNING;
        while self.ops[self.pc] != 99 && self.state == RUNNING {
            let op = self.ops[self.pc] % 100;

            match op {
                1   => self.op_add(),
                2   => self.op_mul(),
                3   => self.op_cin(),
                4   => self.op_cout(),
                5   => self.op_jump(true),
                6   => self.op_jump(false),
                7   => self.op_lt(),
                8   => self.op_eq(),
                _   => panic!("Unknown opcode: {:?}!", op),
            }

            if self.state == WAITING_FOR_INPUT { return; }
        }
        self.state = FINISHED;
    }

    pub fn result(&self) -> i64 {
        return self.ops[0];
    }

    pub fn is_finished(&self) -> bool {
        return self.state == FINISHED;
    }

    fn get_param(&self, par_num: usize) -> i64 {
        let i = 10_i64.pow((par_num+1) as u32);
        let p_mode = (self.ops[self.pc] / i) % 10;
        let val = self.ops[self.pc+par_num];

        return match p_mode {
            0 => self.ops[val as usize],
            1 => val,
            _ => panic!("Unknown parameter mode")
        }
    }

    fn op_add(&mut self) {
        let a: i64 = self.get_param(1);
        let b: i64 = self.get_param(2);
        let dst: i64 = self.ops[self.pc+3];

        self.ops[dst as usize] = a + b;
        self.pc += 4;
    }

    fn op_mul(&mut self) {
        let a: i64 = self.get_param(1);
        let b: i64 = self.get_param(2);
        let dst: i64 = self.ops[self.pc+3];

        self.ops[dst as usize] = a * b;
        self.pc += 4;
    }

    fn op_cin(&mut self) {
        if let Some(input) = self.io_read() {
            let dst: i64 = self.ops[self.pc+1];
    
            self.ops[dst as usize] = input;
            self.pc += 2;
            return;
        }
        // no input?: don't "UP" the PC, so this op runs again when started
        self.state = WAITING_FOR_INPUT;
    }

    fn op_cout(&mut self) {
        let src: i64 = self.get_param(1);

        self.io_write(src);
        self.pc += 2;
    }

    fn op_jump(&mut self, jump_if: bool) {
        let a: i64 = self.get_param(1);

        if (a != 0) == jump_if {
            let dst: i64 = self.get_param(2);
            self.pc = dst as usize;
            return;
        }

        self.pc += 3;
    }

    fn op_lt(&mut self) {
        let a: i64 = self.get_param(1);
        let b: i64 = self.get_param(2);
        let dst: i64 = self.ops[self.pc+3];

        self.ops[dst as usize] = match a < b {
            true   => 1,
            false  => 0,
        };
        self.pc += 4;
    }

    fn op_eq(&mut self) {
        let a: i64 = self.get_param(1);
        let b: i64 = self.get_param(2);
        let dst: i64 = self.ops[self.pc+3];

        self.ops[dst as usize] = match a == b {
            true   => 1,
            false  => 0,
        };
        self.pc += 4;
    }

    // -- I/O stuff:

    fn io_read(&mut self) -> Option<i64> {
        if self.read_queue.is_empty() {
            return None;
        }

        return self.read_queue.pop_front();
    }

    pub fn stdin(&mut self, val: i64) {
        self.read_queue.push_back(val);
    }

    fn io_write(&mut self, val: i64) {
        self.write_queue.push_back(val);
    }

    pub fn stdout(&mut self) -> Option<i64> {
        return self.write_queue.pop_front();
    }

    #[allow(dead_code)]
    pub fn debug(&self) -> () {
        println!("p: {:?}", self.ops);
        println!("r: {:?}", self.read_queue);
        println!("w: {:?}", self.write_queue);
    }
}

#[cfg(test)]
mod tests {
  use super::*;

  fn compute(program: IntcodeProg) -> IntcodeProg {
    let mut t = Intcode::new(&program);

    t.compute();

    return t.ops;
  }

  #[test]
  fn day02_tests() {
    assert_eq!(vec![2,0,0,0,99], compute(vec![1,0,0,0,99]));
    assert_eq!(vec![2,3,0,6,99], compute(vec![2,3,0,3,99]));
    assert_eq!(vec![2,4,4,5,99,9801], compute(vec![2,4,4,5,99,0]));
    assert_eq!(vec![30,1,1,4,2,5,6,0,99], compute(vec![1,1,1,4,99,5,6,0,99]));
  }

  #[test]
  fn jump_tests() {
    // Position mode jump
    let prog_jump_pos = vec![3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9];

    let mut t = Intcode::new(&prog_jump_pos);
    t.stdin(0);
    t.compute();
    assert_eq!(0, t.stdout().unwrap());

    let mut t = Intcode::new(&prog_jump_pos);
    t.stdin(-17);
    t.compute();
    assert_eq!(1, t.stdout().unwrap());


    // Immediate mode jump
    let prog_jump_imm = vec![3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9];
    
    t = Intcode::new(&prog_jump_imm);
    t.stdin(0);
    t.compute();
    assert_eq!(0, t.stdout().unwrap());

    let mut t = Intcode::new(&prog_jump_imm);
    t.stdin(-17);
    t.compute();
    assert_eq!(1, t.stdout().unwrap());


    // More complicated Jump example
    let prog_jump = vec![3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
    1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
    999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99];

    t = Intcode::new(&prog_jump);
    t.stdin(1);
    t.compute();
    assert_eq!(999, t.stdout().unwrap());

    t = Intcode::new(&prog_jump);
    t.stdin(8);
    t.compute();
    assert_eq!(1000, t.stdout().unwrap());

    t = Intcode::new(&prog_jump);
    t.stdin(11);
    t.compute();
    assert_eq!(1001, t.stdout().unwrap());
  }
}