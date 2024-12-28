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
    fn compute(&self, a: u32, b: u32) -> u32 {
        match self {
            Operand::AND => a & b,
            Operand::OR => a | b,
            Operand::XOR => a ^ b,
        }
    }
}

#[derive(Debug, Clone, PartialOrd, PartialEq, Ord, Eq)]
enum Wire {
    Value(u32),
    Gate(Operand, String, String),
}

impl Wire {
    fn value(&self, gates: &HashMap<String, Wire>) -> u32 {
        match self {
            Wire::Value(v) => *v,
            Wire::Gate(op, wire1, wire2) => {
                op.compute(gates[wire1].value(gates), gates[wire2].value(gates))
            }
        }
    }

    fn operand(&self) -> Operand {
        match self {
            Wire::Value(_) => { panic!("Not a gate Wire!") }
            Wire::Gate(op, _, _) => { *op }
        }
    }

    fn get_inputs(&self) -> Vec<&String> {
        match self {
            Wire::Value(_) => { panic!("Not a gate Wire!") }
            Wire::Gate(_, a, b) => { vec![a, b] }
        }
    }
}

fn evaluate(wires: &Vec<String>, gates: &HashMap<String, Wire>) -> u64 {
    let mut sum = 0;
    for (i, zwire) in wires.iter().enumerate() {
        sum += gates[zwire].value(gates) as u64 * 2u64.pow(i as u32)
    }

    sum
}

fn named_wires(prefix: &str, gates: &HashMap<String, Wire>) -> Vec<String> {
    let mut wires = gates
        .keys()
        .filter(|&x| x.starts_with(prefix))
        .cloned()
        .collect::<Vec<_>>();
    wires.sort();

    wires
}

fn main() -> Result<(), ()> {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();

    let mut gates = HashMap::new();
    let mut input_to_tgt: HashMap<String, Vec<String>> = HashMap::new();

    for line in input.lines() {
        if line.contains(":") {
            let (gate, value) = line.split_once(":").unwrap();
            gates.insert(
                gate.trim().to_string(),
                Wire::Value(value.trim().parse().unwrap()),
            );
        } else if line.contains("->") {
            let parts = line.split_ascii_whitespace().collect::<Vec<_>>();
            let a = parts[0].to_string();
            let b = parts[2].to_string();
            let target = parts[4].to_string();

            gates.insert(target.clone(), Wire::Gate(parts[1].parse()?, a.clone(), b.clone()));

            input_to_tgt.entry(a.clone()).or_default().push(target.clone());
            input_to_tgt.entry(b.clone()).or_default().push(target.clone());
        }
    }

    let z_wires = named_wires("z", &gates);
    let zval = evaluate(&z_wires, &gates);

    println!("Part 1: {}", zval);

    let mut faulty_gates = Vec::new();

    for (name, gate) in &gates {
        if name.starts_with("z") {
            if gate.operand() != Operand::XOR {
                // special last case
                if z_wires[z_wires.len()-1] == *name && gate.operand() == Operand::OR { continue; }
                faulty_gates.push(name.clone());
            }
            continue;
        } else if name.starts_with("x") || name.starts_with("y") {
            continue;
        }

        if gate.operand() == Operand::OR {
            let inputs = gate.get_inputs();
            for &faulty in inputs.iter().filter(|&&x| gates[x].operand() != Operand::AND) {
                faulty_gates.push(faulty.clone());
            }
        }

        if gate.operand() == Operand::AND {
            let inputs = gate.get_inputs();

            // special first case:
            if inputs.iter().all(|&x| x.ends_with("00")) { continue; }

            let target = &input_to_tgt[name];
            if target.len() != 1 {
                faulty_gates.push(name.clone());
            }

            if !inputs.iter().all(|&x| x.ends_with("00")) && gates[&target[0]].operand() != Operand::OR {
                faulty_gates.push(name.clone());
            }
        }

        if gate.operand() == Operand::XOR {
            let inputs = gate.get_inputs();

            // special first case:
            if inputs.iter().all(|&x| x.ends_with("00")) { continue; }

            let mut target = input_to_tgt[name].iter().map(|x| (gates[x].clone(), x.clone())).collect::<Vec<_>>();
            target.sort();

            let is_x_input = inputs.iter().all(|&input_wire| input_wire.starts_with("x") || input_wire.starts_with("y"));
            if is_x_input {
                if target.len() != 2 || target[0].0.operand() != Operand::XOR || target[1].0.operand() != Operand::AND {
                    faulty_gates.push(name.clone());
                }
            }
            if !is_x_input {
                // non-z gate cannot be a grandparent of x/y (that is reserved for z-nodes)
                if inputs.iter().any(|&x| gates[x].get_inputs().iter().all(|&x| x.starts_with("x") || x.starts_with("y"))) {
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
