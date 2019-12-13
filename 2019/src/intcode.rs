use std::collections::VecDeque;

type IntcodeType = i64;
pub type IntcodeProg = Vec<IntcodeType>;

const FINISHED: u8 = 0;
const RUNNING: u8 = 1;
const WAITING_FOR_INPUT: u8 = 2;

pub struct Intcode {
    pc: usize,
    ops: IntcodeProg,
    read_queue: VecDeque<IntcodeType>,
    write_queue: VecDeque<IntcodeType>,
    state: u8,
    relative_base: IntcodeType,
}

impl Intcode {
    pub fn new(program: &IntcodeProg) -> Intcode {
        Intcode {
            pc: 0,
            ops: program.clone(),
            read_queue: VecDeque::new(),
            write_queue: VecDeque::new(),
            state: RUNNING,
            relative_base: 0,
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
                5   => self.op_jump(|a| a != 0),
                6   => self.op_jump(|a| a == 0),
                7   => self.op_cmp(|a,b| a < b),
                8   => self.op_cmp(|a,b| a == b),
                9   => self.op_adjust_rel_base(),
                _   => panic!("Unknown opcode: {:?}!", op),
            }

            if self.state == WAITING_FOR_INPUT { return; }
        }
        self.state = FINISHED;
    }

    pub fn result(&self) -> i64 {
        return self.ops[0] as i64;
    }

    pub fn is_finished(&self) -> bool {
        return self.state == FINISHED;
    }

    pub fn is_waiting(&self) -> bool {
        return self.state == WAITING_FOR_INPUT;
    }

    pub fn has_output(&self) -> bool {
        return self.write_queue.len() != 0;
    }

    fn get_val(&mut self, pos: usize) -> IntcodeType {
        if pos >= self.ops.len() {
            self.ops.resize(pos+1, 0);
            return 0;
        }
        return self.ops[pos];
    }

    fn get_param(&self, par_num: usize) -> usize {
        let i = 10_i64.pow((par_num+1) as u32);
        let p_mode = (self.ops[self.pc] / i) % 10;
        let index = self.pc+par_num;
        let val = self.ops[index];

        return match p_mode {
            0 => val as usize,
            1 => index,
            2 => (self.relative_base + val) as usize,
            _ => panic!("Unknown parameter mode")
        }
    }

    fn set_val(&mut self, pos: usize, val: IntcodeType) -> () {
        // println!("p: {}, v:{}", pos, val);
        if pos >= self.ops.len() {
            self.ops.resize(pos+1, 0);
        }
        self.ops[pos] = val;
    }

    fn op_add(&mut self) {
        let a: IntcodeType = self.get_val(self.get_param(1));
        let b: IntcodeType = self.get_val(self.get_param(2));
        let dst: usize = self.get_param(3);

        self.set_val(dst, a + b);
        self.pc += 4;
    }

    fn op_mul(&mut self) {
        let a: IntcodeType = self.get_val(self.get_param(1));
        let b: IntcodeType = self.get_val(self.get_param(2));
        let dst: usize = self.get_param(3);

        self.set_val(dst, a * b);
        self.pc += 4;
    }

    fn op_cin(&mut self) {
        if let Some(input) = self.io_read() {
            let dst: usize = self.get_param(1);
    
            self.set_val(dst, input);
            self.pc += 2;
            return;
        }
        // no input?: don't "UP" the PC, so this op runs again when started
        self.state = WAITING_FOR_INPUT;
    }

    fn op_cout(&mut self) {
        let src: IntcodeType = self.get_val(self.get_param(1));

        self.io_write(src);
        self.pc += 2;
    }

    fn op_jump<F>(&mut self, predicate: F) -> () where F : Fn(IntcodeType) -> bool {
        let a: IntcodeType = self.get_val(self.get_param(1));

        if predicate(a) {
            self.pc = self.get_val(self.get_param(2)) as usize;
            return;
        }

        self.pc += 3;
    }

    fn op_cmp<F>(&mut self, predicate: F) -> () where F : Fn(IntcodeType, IntcodeType) -> bool {
        let a: IntcodeType = self.get_val(self.get_param(1));
        let b: IntcodeType = self.get_val(self.get_param(2));
        let dst: usize = self.get_param(3);

        if predicate(a,b) {
            self.set_val(dst, 1);
        } else {
            self.set_val(dst, 0);
        }
        self.pc += 4;
    }

    fn op_adjust_rel_base(&mut self) {
        let a: IntcodeType = self.get_val(self.get_param(1));

        self.relative_base += a;
        self.pc += 2;
    }

    // -- I/O stuff:

    fn io_read(&mut self) -> Option<IntcodeType> {
        if self.read_queue.is_empty() {
            return None;
        }

        return self.read_queue.pop_front();
    }

    pub fn stdin(&mut self, val: i64) {
        self.read_queue.push_back(val);
    }

    fn io_write(&mut self, val: IntcodeType) {
        // println!("{}", val);
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
        println!("s: {:?}", self.state);
        println!("b: {:?}", self.relative_base);
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
  fn jump_position_mode_tests() {
    // Position mode jump
    let prog_jump_pos = vec![3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9];

    let mut t = Intcode::new(&prog_jump_pos);
    t.stdin(0);
    t.compute();
    assert_eq!(0, t.stdout().unwrap());

    t = Intcode::new(&prog_jump_pos);
    t.stdin(-17);
    t.compute();
    assert_eq!(1, t.stdout().unwrap());
  }

  #[test]
  fn jump_immediate_mode_tests() {
    // Immediate mode jump
    let prog_jump_imm = vec![3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9];

    let mut t = Intcode::new(&prog_jump_imm);
    t.stdin(0);
    t.compute();
    assert_eq!(0, t.stdout().unwrap());

    t = Intcode::new(&prog_jump_imm);
    t.stdin(-17);
    t.compute();
    assert_eq!(1, t.stdout().unwrap());
  }

  #[test]
  fn jump_complicated_example_tests() {
    // More complicated Jump example
    let prog_jump = vec![3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
    1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
    999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99];

    let mut t = Intcode::new(&prog_jump);
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

  #[test]
  fn quine_test()
  {
      let prog = vec![109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99];
      let mut t = Intcode::new(&prog);
      t.compute();

      let mut i = 0;
      while let Some(x) = t.stdout() {
        assert_eq!(prog[i], x);
        i += 1;
      }
  }

  #[test]
  fn test_bigint()
  {
        let prog = vec![1102,34915192,34915192,7,4,7,99,0];
        let mut t = Intcode::new(&prog);
        t.compute();

        assert_eq!(1219070632396864, t.stdout().unwrap());
  }

  #[test]
  fn test_bigint2()
  {
    let prog = vec![104,1125899906842624,99];
    let mut t = Intcode::new(&prog);
    t.compute();

    assert_eq!(1125899906842624, t.stdout().unwrap());
  }
}