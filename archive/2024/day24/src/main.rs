use std::collections::HashMap;
use std::fmt::Debug;
use std::io;
use std::io::Read;
use std::str::FromStr;

#[derive(Debug, PartialEq, Copy, Clone, PartialOrd, Ord, Eq)]
enum Operand {
    XOR,
    AND,
    OR,
}

impl FromStr for Operand {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "XOR" => Ok(Operand::XOR),
            "AND" => Ok(Operand::AND),
            "OR" => Ok(Operand::OR),
            _ => Err(()),
        }
    }
}

impl Operand {
    fn compute(&self, a: u8, b: u8) -> u8 {
        match self {
            Operand::AND => a & b,
            Operand::OR => a | b,
            Operand::XOR => a ^ b,
        }
    }
}

#[derive(Debug, Clone, PartialOrd, PartialEq, Ord, Eq)]
struct Gate {
    a: String,
    b: String,
    op: Operand,
    tgt: String,
}

impl Gate {
    fn from_input(input: &str) -> Self {
        let parts = input.split_ascii_whitespace().collect::<Vec<_>>();
        let a = parts[0].to_string();
        let b = parts[2].to_string();
        let tgt = parts[4].to_string();

        let (a, b) = if a < b { (a, b) } else { (b, a) };

        Gate {
            a,
            b,
            tgt,
            op: parts[1].parse().unwrap(),
        }
    }

    fn value(&self, inputvalues: &HashMap<String, u8>, gates: &HashMap<String, Gate>) -> u8 {
        let a = if let Some(&inwire) = inputvalues.get(&self.a) {
            inwire
        } else { gates[&self.a].value(inputvalues, gates) };
        let b = if let Some(&inwire) = inputvalues.get(&self.b) {
            inwire
        } else { gates[&self.b].value(inputvalues, gates) };

        self.op.compute(a, b)
    }
}

fn main() -> Result<(), ()> {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();

    let mut inputvalues = HashMap::new();
    let mut gates = HashMap::new();
    let mut input_to_tgt: HashMap<String, Vec<String>> = HashMap::new();

    let mut x_wires = Vec::new();
    let mut z_wires = Vec::new();

    for line in input.lines() {
        if line.contains(":") {
            let (wire, value) = line.split_once(":").unwrap();
            inputvalues.insert(wire.trim().to_string(), value.trim().parse::<u8>().unwrap());

            if wire.starts_with("x") {
                x_wires.push(wire);
            }

        } else if line.contains("->") {
            let gate = Gate::from_input(&line);

            if gate.tgt.starts_with("z") {
                z_wires.push(gate.tgt.clone());
            }

            let tgt = &gate.tgt;

            input_to_tgt.entry(gate.a.clone()).or_default().push(tgt.clone());
            input_to_tgt.entry(gate.b.clone()).or_default().push(tgt.clone());
            gates.insert(tgt.clone(), gate);
        }
    }

    x_wires.sort();
    z_wires.sort();

    // Part 1:
    let mut compute = 0;
    for (i, zwire) in z_wires.iter().enumerate() {
        compute += gates[zwire].value(&inputvalues, &gates) as u64 * 2u64.pow(i as u32)
    }

    println!("Part 1: {}", compute);

    let mut faulty_gates = Vec::new();
    for (name, gate) in &gates {
        let inputs = [&gate.a, &gate.b];

        if name.starts_with("z") {
            if gate.op != Operand::XOR {
                // special last case
                if z_wires[z_wires.len()-1] == *name && gate.op == Operand::OR { continue; }
                faulty_gates.push(name.clone());
            }
            continue;
        }

        if gate.op == Operand::OR {
            for &faulty in inputs.iter().filter(|&&x| gates[x].op != Operand::AND) {
                faulty_gates.push(faulty.clone());
            }
        }

        if gate.op == Operand::AND {
            // special first case:
            if inputs.iter().all(|&x| x.ends_with("00")) { continue; }

            let targets = &input_to_tgt[name];
            if targets.len() != 1 {
                faulty_gates.push(name.clone());
            }

            // end gates always go to OR gates
            if gates[&targets[0]].op != Operand::OR {
                faulty_gates.push(name.clone());
            }
        }

        if gate.op == Operand::XOR {
            // special first case:
            if inputs.iter().all(|&x| x.ends_with("00")) { continue; }

            let mut targets = input_to_tgt[name].iter().map(|x| (gates[x].clone(), x.clone())).collect::<Vec<_>>();
            targets.sort();

            let is_input_gate = inputs[0].starts_with("x") && inputs[1].starts_with("y");
            if is_input_gate {
                if targets.len() != 2 || targets[0].0.op != Operand::XOR || targets[1].0.op != Operand::AND {
                    faulty_gates.push(name.clone());
                }
            }
            if !is_input_gate {
                // non-z gate cannot be a grandparent of x/y (that is reserved for z-nodes)
                if inputs.iter().any(|&x| gates[x].a.starts_with("x") && gates[x].b.starts_with("y")) {
                    faulty_gates.push(name.clone());
                }
            }
        }
    }

    faulty_gates.sort();
    faulty_gates.dedup();

    println!("Part 2: {}", faulty_gates.join(","));
    Ok(())
}
