use crate::Day;

pub struct Day10 {
    pub input: Vec<String>,
}

fn opposite(a: char) -> char {
    match a {
        '(' => ')',
        '[' => ']',
        '{' => '}',
        '<' => '>',
        _ => panic!("No opposite known for"),
    }
}

fn matches(a: char, b: char) -> bool {
    b == opposite(a)
}

fn get_char_score_corruption(a: char) -> i64 {
    match a {
        ')' => 3,
        ']' => 57,
        '}' => 1197,
        '>' => 25137,
        _ => panic!("No score known for"),
    }
}

fn get_char_score_autocomplete(a: char) -> i64 {
    match a {
        ')' => 1,
        ']' => 2,
        '}' => 3,
        '>' => 4,
        _ => panic!("No score known for"),
    }
}

fn get_corruption_score(line: &String) -> i64 {
    let mut score = 0;
    let mut stack = Vec::new();
    
    for c in line.chars() {
        match c {
            '{' | '(' | '[' | '<' => stack.push(c),
            _ => {
                let p = stack.pop();
                if p != None && !matches(p.unwrap(), c) {
                    score += get_char_score_corruption(c);
                }
            },
        }
    }
    
    score
}

fn get_autocomplete_score(line: &String) -> i64 {
    let mut score = 0;
    let mut stack = Vec::new();

    for c in line.chars() {
        match c {
            '{' | '(' | '[' | '<' => stack.push(c),
            _ => { let _ = stack.pop(); },
        }
    }

    while let Some(c) = stack.pop() {
        let sc = get_char_score_autocomplete(opposite(c));
        score *= 5;
        score += sc;
    }

    score
}

impl Day for Day10 {
    // naive implementation.. Didn't cut it for part 2
    fn part1(&self) -> String {
        self.input.iter()
            .map(|l| get_corruption_score(l))
            .sum::<i64>()
            .to_string()
    }
    
    fn part2(&self) -> String {
        let mut lines = self.input.clone();
        lines.retain(|l| 0 == get_corruption_score(l));

        let mut scores: Vec<i64> = lines.iter()
            .map(|l| get_autocomplete_score(l))
            .collect();

        scores.sort();
        
        scores[scores.len() / 2].to_string()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    const INPUT: &str = r#"[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]"#;

    fn get_day(input_num: u8) -> Day10 {
        let inp = match input_num {
            0 => include_str!("../inputs/day10.txt"),
            1 => INPUT,
            _ => panic!(),
        };

        Day10 {
            input: inp.split('\n').map(|s| s.to_string()).collect(),
        }
    }

    #[test]
    fn part1_example() {
        let d = get_day(1);

        assert_eq!("26397", d.part1());
    }

    #[test]
    fn part2_example() {
        let d = get_day(1);

        assert_eq!("288957", d.part2());
    }

    #[test]
    fn part1() {
        let d = get_day(0);

        assert_eq!("366027", d.part1());
    }

    #[test]
    fn part2() {
        let d = get_day(0);

        assert_eq!("1118645287", d.part2());
    }
}
