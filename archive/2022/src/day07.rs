use std::collections::{HashMap, HashSet};
use crate::Day;

pub struct Day07 {
    pub input: Vec<String>,
}

type FileSystem = HashMap<String, Entry>;

struct Directory {
    entries: HashSet<String>
}

enum Entry {
    Dir(Directory),
    File(usize),
}

impl Directory {
    fn new() -> Directory {
        Directory {
            entries: HashSet::new()
        }
    }

    fn insert_entry(&mut self, filename: String) {
        self.entries.insert(filename);
    }
}

fn parse_shell(input: &Vec<String>) -> FileSystem {
    let mut entitypool = HashMap::new();
    entitypool.insert("/".to_string(), Entry::Dir(Directory::new()));

    let mut cwd = "/".to_string();

    for command in input {
        if let Entry::Dir(dir) = entitypool.get_mut(&cwd).unwrap() {
            if command.starts_with("$ cd ") {
                let name: String = command.clone().drain(5..).collect();
                if name == "/" {
                    cwd = "/".to_string();
                    continue;
                }
                if name == ".." {
                    let (new_cwd, _) = cwd.trim_end_matches('/').rsplit_once('/').unwrap();
                    cwd = new_cwd.to_string() + "/";
                    continue;
                }

                cwd += &name;
                cwd += "/";

                dir.insert_entry(cwd.clone());
                entitypool.insert(cwd.clone(), Entry::Dir(Directory::new()));
            } else if !command.starts_with('$') {
                let spl: Vec<&str> = command.split_whitespace().collect();
                let filesize = spl[0];

                if filesize == "dir" {
                    // ignore this dir, if it has size we'll visit it later
                    continue;
                }

                let filename: String = cwd.clone() + spl[1];

                dir.insert_entry(filename.clone());

                entitypool.insert(filename, Entry::File(filesize.parse().unwrap()));
            }
        }
        else {
            panic!("nooooo!");
        }
    }

    entitypool
}

fn get_size(fs: &FileSystem, dir: &str) -> usize {
    match fs.get(dir) {
        None => panic!("dead reference!"),
        Some(Entry::File(s)) => *s,
        Some(Entry::Dir(d)) => d.entries.iter().map(|e| get_size(fs, e)).sum()
    }
}

fn visit_dirs<T>(fs: &FileSystem, dir: &str, mut f: T) where T: FnMut(&str) {
    let mut stack = vec![dir];

    while !stack.is_empty() {
        let node = stack.pop().unwrap();
        if let Some(Entry::Dir(dir)) = fs.get(node) {

            f(node);

            for entry in &dir.entries {
                stack.push(entry);
            }
        }
    }
}

impl Day for Day07 {
    fn part1(&self) -> String {
        let filesystem = parse_shell(&self.input);
        let mut sumsizes = 0;

        visit_dirs(&filesystem, "/", |dirname| {
            let nodesize = get_size(&filesystem, dirname);

            if nodesize <= 100000 {
                sumsizes += nodesize;
            }
        });

        sumsizes.to_string()
    }

    fn part2(&self) -> String {
        const MAX: usize = 70000000;
        const MIN: usize = 30000000;

        let filesystem = parse_shell(&self.input);
        let cursize = get_size(&filesystem, "/");
        let desired_win = MIN - (MAX - cursize);

        let mut bestsize = MIN;
        visit_dirs(&filesystem, "/", |dirname| {
            let nodesize = get_size(&filesystem, dirname);

            if nodesize < bestsize && nodesize > desired_win {
                bestsize = nodesize;
            }
        });

        bestsize.to_string()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    const INPUT: &str = "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k";

    fn get_day(input_num: u8) -> Day07 {
        let inp = match input_num {
            0 => include_str!("../inputs/day07.txt"),
            1 => INPUT,
            _ => panic!(),
        };

        Day07 {
            input: inp.lines().map(|s| s.to_string()).collect(),
        }
    }

    #[test]
    fn part1_example() {
        let d = get_day(1);

        assert_eq!("95437", d.part1());
    }

    #[test]
    fn part2_example() {
        let d = get_day(1);

        assert_eq!("24933642", d.part2());
    }

    #[test]
    fn part1() {
        let d = get_day(0);

        assert_eq!("1908462", d.part1());
    }

    #[test]
    fn part2() {
        let d = get_day(0);

        assert_eq!("3979145", d.part2());
    }
}
