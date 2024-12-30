use aoc_util::PuzzleDay;

pub struct Day02 {
    lists: Vec<Vec<i32>>,
}

impl PuzzleDay for Day02 {
    fn new(input: &str) -> Self {
        let mut lists: Vec<Vec<i32>> = Vec::new();
        for line in input.lines() {
            lists.push(line.split_ascii_whitespace().map(|s| s.parse().unwrap()).collect());
        }

        Day02 { lists }
    }

    fn solve(&mut self) -> Result<(String, String), String> {
        let mut count_valid1 = 0;
        let mut count_valid2 = 0;
        for list in &self.lists {
            if growing_or_shrinking(list) {
                count_valid1 += 1;
                count_valid2 += 1;
            } else {
                for reduced_list in reduce_lists(list) {
                    if growing_or_shrinking(&reduced_list) {
                        count_valid2 += 1;
                        break;
                    }
                }
            }
        }

        Ok((count_valid1.to_string(), count_valid2.to_string()))
    }
}

fn growing_or_shrinking(list: &Vec<i32>) -> bool {
    let growing = ! list.windows(2)
        .any(|w| w[0] >= w[1] || w[0].abs_diff(w[1]) > 3);
    let shrinking = ! list.windows(2)
        .any(|w| w[0] <= w[1] || w[0].abs_diff(w[1]) > 3);

    growing || shrinking
}


fn reduce_lists(list: &Vec<i32>) -> Vec<Vec<i32>> {
    let mut result = Vec::new();
    for i in 0..list.len() {
        let mut newlist = list.clone();
        newlist.remove(i);
        result.push(newlist);
    }

    result
}
