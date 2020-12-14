extern crate regex;

use self::regex::Regex;
use super::Day;
use util::ceil_divide;
use std::collections::HashMap;
use std::collections::VecDeque;

pub struct Day14 {
  recipes: HashMap<String, Recipe>,
}

struct Recipe {
  amount: i64,
  ingredients: Vec<(i64, String)>,
}

fn get_recipes(input: &str) -> HashMap<String, Recipe> {
  lazy_static! {
    static ref PROD: Regex = Regex::new(r"(.*) => (\d+) ([A-Z]+)").unwrap();
    static ref INGR: Regex = Regex::new(r"(\d+) ([A-Z]+)").unwrap();
  }

  let lines: Vec<&str> = input.lines().collect();

  let mut recipes: HashMap<String, Recipe> = HashMap::new();
  for line in lines {
    let mut ingredients: Vec<(i64, String)> = Vec::new();

    let produce_capt = PROD.captures(line).unwrap();
    let produce = String::from(&produce_capt[3]);

    for ingredient in INGR.captures_iter(&produce_capt[1]) {
      ingredients.push((ingredient[1].parse().unwrap(), String::from(&ingredient[2])));
    }


    recipes.insert(produce, Recipe {
      amount: produce_capt[2].parse().unwrap(),
      ingredients: ingredients,
    });
  }
  recipes
}

impl Day for Day14 {
  fn new(input: &str) -> Day14 {
    Day14 {
      recipes: get_recipes(input),
    }
  }

  fn part1(&self) -> Box<dyn std::fmt::Display> {
    return Box::new(self.run1());
  }

  fn part2(&self) -> Box<dyn std::fmt::Display> {
    return Box::new(self.run2());
  }
}

// -- Privates
impl Day14 {
    fn run1(&self) -> i64 {
      self.how_much_ore_for_fuel(1)
    }

    fn how_much_ore_for_fuel(&self, fuel_amount: i64) -> i64 {
      let mut todo = HashMap::new();
      todo.insert(String::from("FUEL"), fuel_amount);

      self.recursive_todo(&mut todo)
    }

    fn is_a_dependency(&self, key: &String, ingredient: &String) -> bool {
      if key == "ORE" { return false }

      let recipe = self.recipes.get(key).unwrap();

      for (_, ingr) in &recipe.ingredients {
        if ingredient == ingr ||
          self.is_a_dependency(ingr, ingredient)
        {
          return true;
        }
      }

      false
    }

    fn pop_first_independent(&self, todolist: &mut HashMap<String, i64>) -> (String, i64) {
      let mut iter: VecDeque<String> = todolist.keys().map(|x| x.to_string()).collect();
      let found_elem: String;
      loop {
        let elem = iter.pop_front().unwrap();
        if iter.is_empty() || iter.iter().all(|x| !self.is_a_dependency(x, &elem)) {
          found_elem = elem;
          break;
        }
        iter.push_back(elem);
      }

      todolist.remove_entry(&found_elem).unwrap()
    }

    fn recursive_todo(&self, todolist: &mut HashMap<String, i64>) -> i64 {
      if todolist.is_empty() {
        return 0;
      }

      let mut needed_ore = 0;

      let (ingredient, amount_needed) = self.pop_first_independent(todolist);
      let recipe = self.recipes.get(&ingredient).unwrap();

      let recipe_times: i64 = ceil_divide(amount_needed, recipe.amount);

      for (amount, ingr) in &recipe.ingredients {
        let needed = amount * recipe_times;
        if ingr == "ORE" {
          needed_ore += needed;
          continue;
        }
        if let Some(todo) = todolist.get_mut(ingr) {
          *todo += needed;
        } else {
          todolist.insert(ingr.to_string(), needed);
        }
      }

      needed_ore + self.recursive_todo(todolist)
    }

    fn run2(&self) -> i64 {
      let have_ore = 1000000000000;

      let ore_for_one_fuel = self.how_much_ore_for_fuel(1);
      let mut fuel_amount = have_ore / ore_for_one_fuel - 1;

      loop {
        let new_ore_needed = self.how_much_ore_for_fuel(fuel_amount);

        if new_ore_needed > have_ore {
          return fuel_amount - 1;
        } else if new_ore_needed == have_ore {
          return fuel_amount;
        }

        let fuel_add = (have_ore - new_ore_needed) / ore_for_one_fuel;

        if fuel_add == 0 {
          fuel_amount += 1;
        } else {
          fuel_amount += fuel_add;
        }

      }
    }
}

#[cfg(test)]
mod test {
  use super::*;

  const EXAMPLE1: &str = "10 ORE => 10 A
1 ORE => 1 B
7 A, 1 B => 1 C
7 A, 1 C => 1 D
7 A, 1 D => 1 E
7 A, 1 E => 1 FUEL";

  const EXAMPLE2: &str = "9 ORE => 2 A
8 ORE => 3 B
7 ORE => 5 C
3 A, 4 B => 1 AB
5 B, 7 C => 1 BC
4 C, 1 A => 1 CA
2 AB, 3 BC, 4 CA => 1 FUEL";

  const EXAMPLE3: &str = "157 ORE => 5 NZVS
165 ORE => 6 DCFZ
44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL
12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ
179 ORE => 7 PSHF
177 ORE => 5 HKGWZ
7 DCFZ, 7 PSHF => 2 XJWVT
165 ORE => 2 GPVTF
3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT";

  const EXAMPLE4: &str = "2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG
17 NVRVD, 3 JNWZP => 8 VPVL
53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL
22 VJHF, 37 MNCFX => 5 FWMGM
139 ORE => 4 NVRVD
144 ORE => 7 JNWZP
5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC
5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV
145 ORE => 6 MNCFX
1 NVRVD => 8 CXFTF
1 VJHF, 6 MNCFX => 4 RFSQX
176 ORE => 6 VJHF";

  const EXAMPLE5: &str = "171 ORE => 8 CNZTR
7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL
114 ORE => 4 BHXH
14 VRPVC => 6 BMBT
6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL
6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT
15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW
13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW
5 BMBT => 4 WPTQ
189 ORE => 9 KTJDG
1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP
12 VRPVC, 27 CNZTR => 2 XDBXC
15 KTJDG, 12 BHXH => 5 XCVML
3 BHXH, 2 VRPVC => 7 MZWV
121 ORE => 7 VRPVC
7 XCVML => 6 RJRHP
5 BHXH, 4 VRPVC => 5 LTCX";

  #[test]
  fn part1_example1()
  {
    let d14 = Day14::new(EXAMPLE1);

    assert_eq!(d14.run1(), 31);
  }

  #[test]
  fn part1_example2()
  {
    let d14 = Day14::new(EXAMPLE2);

    assert_eq!(d14.run1(), 165);
  }

  #[test]
  fn part1_example3() {
    let d14 = Day14::new(EXAMPLE3);

    assert_eq!(d14.run1(), 13312);
  }

  #[test]
  fn part1_example4() {
    let d14 = Day14::new(EXAMPLE4);

    assert_eq!(d14.run1(), 180697);
  }

  #[test]
  fn part1_example5() {
    let d14 = Day14::new(EXAMPLE5);

    assert_eq!(d14.run1(), 2210736);
  }

  #[test]
  fn part2_example3() {
    let d14 = Day14::new(EXAMPLE3);

    assert_eq!(d14.run2(), 82892753);
  }

  #[test]
  fn part2_example4() {
    let d14 = Day14::new(EXAMPLE4);

    assert_eq!(d14.run2(), 5586022);
  }

  #[test]
  fn part2_example5() {
    let d14 = Day14::new(EXAMPLE5);

    assert_eq!(d14.run2(), 460664);
  }

  #[test]
  fn part1() {
    let d14 = Day14::new(include_str!("../inputs/day14.txt"));

    assert_eq!(d14.run1(), 337862);
  }

  #[test]
  fn part2() {
    let d14 = Day14::new(include_str!("../inputs/day14.txt"));

    assert_eq!(d14.run2(), 3687786);
  }

} 