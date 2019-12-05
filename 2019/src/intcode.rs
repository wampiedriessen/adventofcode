use std::collections::VecDeque;

pub struct Intcode {
    pc: usize,
    ops: Vec<i32>,
    read_queue: VecDeque<i32>,
    write_queue: VecDeque<i32>,
}

impl Intcode {
    pub fn new(program: Vec<i32>) -> Intcode {
        Intcode {
            pc: 0,
            ops: program,
            read_queue: VecDeque::new(),
            write_queue: VecDeque::new(),
        }
    }
    
    pub fn read_input(input:&str) -> Vec<i32> {
        let mut vec: Vec<i32> = Vec::new();
        for op in input.split(",") {
            vec.push(op.parse().unwrap())
        }
        return vec;
    }

    pub fn compute(&mut self) -> () {
        while self.ops[self.pc] != 99 {
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
        }
    }

    pub fn result(&self) -> i32 {
        return self.ops[0];
    }

    fn get_param(&self, par_num: usize) -> i32 {
        let i = 10_i32.pow((par_num+1) as u32);
        let p_mode = (self.ops[self.pc] / i) % 10;
        let val = self.ops[self.pc+par_num];

        return match p_mode {
            0 => self.ops[val as usize],
            1 => val,
            _ => panic!("Unknown parameter mode")
        }
    }

    fn op_add(&mut self) {
        let a: i32 = self.get_param(1);
        let b: i32 = self.get_param(2);
        let dst: i32 = self.ops[self.pc+3];

        self.ops[dst as usize] = a + b;
        self.pc += 4;
    }

    fn op_mul(&mut self) {
        let a: i32 = self.get_param(1);
        let b: i32 = self.get_param(2);
        let dst: i32 = self.ops[self.pc+3];

        self.ops[dst as usize] = a * b;
        self.pc += 4;
    }

    fn op_cin(&mut self) {
        let dst: i32 = self.ops[self.pc+1];

        self.ops[dst as usize] = self.io_read().unwrap();
        self.pc += 2;
    }

    fn op_cout(&mut self) {
        let src: i32 = self.get_param(1);

        self.io_write(src);
        self.pc += 2;
    }

    fn op_jump(&mut self, jump_if: bool) {
        let a: i32 = self.get_param(1);

        if (a != 0) == jump_if {
            let dst: i32 = self.get_param(2);
            self.pc = dst as usize;
            return;
        }

        self.pc += 3;
    }

    fn op_lt(&mut self) {
        let a: i32 = self.get_param(1);
        let b: i32 = self.get_param(2);
        let dst: i32 = self.ops[self.pc+3];

        self.ops[dst as usize] = match a < b {
            true   => 1,
            false  => 0,
        };
        self.pc += 4;
    }

    fn op_eq(&mut self) {
        let a: i32 = self.get_param(1);
        let b: i32 = self.get_param(2);
        let dst: i32 = self.ops[self.pc+3];

        self.ops[dst as usize] = match a == b {
            true   => 1,
            false  => 0,
        };
        self.pc += 4;
    }


    // -- I/O stuff:

    fn io_read(&mut self) -> Option<i32> {
        if self.read_queue.is_empty() {
            panic!("No input was given to intcode");
        }

        return self.read_queue.pop_front();
    }

    fn io_write(&mut self, val: i32) {
        self.write_queue.push_back(val);
    }

    pub fn stdin(&mut self, val: i32) {
        self.read_queue.push_back(val);
    }

    pub fn stdout(&mut self) -> Option<i32> {
        return self.write_queue.pop_front();
    }

    pub fn debug(&self) -> () {
        println!("p: {:?}", self.ops);
        println!("r: {:?}", self.read_queue);
        println!("w: {:?}", self.write_queue);
    }
}

#[cfg(test)]
mod tests {
  use super::*;

  fn compute(program: Vec<i32>) -> Vec<i32> {
    let mut t = Intcode::new(program);

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
}