use std::collections::HashMap;
use std::io;
use std::io::Read;

const NUMPAD: &'static str = "789\n456\n123\n 0A";

const DIRPAD: &'static str = " ^A\n<v>";

type CoordMap = HashMap<char, (usize, usize)>;
type Mapper = HashMap<(char, char), Vec<String>>;

struct Bot<'a> {
    pos: char,
    mapper: &'a Mapper,
}

impl Bot<'_> {
    fn get_commands(&self, ch: char) -> &Vec<String> {
        &self.mapper[&(self.pos, ch)]
    }
}

fn get_coords(pad: &'static str) -> CoordMap {
    let mut coords = HashMap::new();
    for (y, line) in pad.lines().enumerate() {
        for (x, ch) in line.chars().enumerate() {
            coords.insert(ch, (y, x));
        }
    }

    coords
}

fn construct_mapper(coords: &CoordMap) -> Mapper {
    let mut mapper = Mapper::new();

    let (spacey, spacex) = coords[&' '];

    for a in coords.keys() {
        if *a == ' ' {
            continue;
        }
        for b in coords.keys() {
            if *b == ' ' {
                continue;
            }

            let (ay, ax) = coords[&a];
            let (by, bx) = coords[&b];

            let mut possibilities = Vec::new();
            let mut todo = vec![(ay, ax, String::new())];

            while let Some((y, x, str)) = todo.pop() {
                if y == spacey && x == spacex {
                    continue;
                }
                if y == by && x == bx {
                    possibilities.push(str);
                    continue;
                }
                if y > by {
                    let mut next = str.clone();
                    next += "^";
                    todo.push((y - 1, x, next))
                }
                if y < by {
                    let mut next = str.clone();
                    next += "v";
                    todo.push((y + 1, x, next))
                }

                if x > bx {
                    let mut next = str.clone();
                    next += "<";
                    todo.push((y, x - 1, next))
                }
                if x < bx {
                    let mut next = str.clone();
                    next += ">";
                    todo.push((y, x + 1, next))
                }
            }

            mapper.insert((*a, *b), possibilities);
        }
    }

    mapper
}

fn main() -> io::Result<()> {
    let numcoords = get_coords(NUMPAD);
    let dircoords = get_coords(DIRPAD);

    let nummmapper = construct_mapper(&numcoords);
    let dirmmapper = construct_mapper(&dircoords);

    let mut input = String::new();
    io::stdin().read_to_string(&mut input)?;

    println!("Part 1: {}", run(&input, &nummmapper, &dirmmapper, 3));
    println!("Part 2: {}", run(&input, &nummmapper, &dirmmapper, 26));

    Ok(())
}

fn run(input: &String, nummapper: &Mapper, dirmapper: &Mapper, steps: usize) -> usize {
    let mut bots = vec![Bot {
        pos: 'A',
        mapper: nummapper,
    }];

    for _ in 1..steps {
        bots.push(Bot {
            pos: 'A',
            mapper: dirmapper,
        });
    }

    let mut memory = HashMap::new();

    let bots = bots.as_mut_slice();

    let mut total: usize = 0;
    for line in input.lines() {
        let mut translation = String::new();
        for ch in line.chars() {
            translation += translate(ch, bots, &mut memory).as_str();
        }

        let sequence = translation.len();

        println!("{}: {} -> {}", line, sequence, translation);

        total += sequence * line[0..3].parse::<usize>().unwrap();
    }

    total
}

fn translate(ch: char, bots: &mut [Bot], memory: &mut HashMap<(usize, String), String>) -> String {
    if bots.len() == 0 {
        return String::from(ch);
    }

    let (bot, nextbots) = bots.split_first_mut().unwrap();
    let commands = bot.get_commands(ch);

    let mut shortest: Option<String> = None;
    for poss in commands {
        let key = (nextbots.len(), poss.clone());
        let translation: String;
        if memory.contains_key(&key) {
            translation = memory[&key].clone();
        } else {
            let mut subbots = String::new();
            for ch in poss.chars() {
                subbots += translate(ch, nextbots, memory).as_str();
            }

            subbots += translate('A', nextbots, memory).as_str();

            memory.insert(key, subbots.clone());
            translation = subbots;
        };

        if let Some(val) = &shortest {
            if translation.len() < val.len() {
                shortest = Some(translation.clone());
            }
            continue;
        }

        shortest = Some(translation.clone());
    }

    bot.pos = ch;
    shortest.unwrap()
}

//
// fn all_possibilities(ch: char, bots: &mut [&mut Bot; 3], depth: usize) -> HashSet<String> {
//     if depth >= bots.len() { return HashSet::new(); }
//     let mut out = HashSet::new();
//
//     for command in bots[depth].get_commands(ch) {
//         let mut newcommand = command.clone();
//         newcommand += "A";
//         out.insert(newcommand);
//     }
//
//     bots[depth].pos = ch;
//
//     out
// }
