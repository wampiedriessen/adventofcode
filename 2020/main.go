package main

import (
    "fmt"
  "day1"
  "day2"
  "day3"
  "day4"
  "day5"
  "day6"
  "day7"
  "day8"
  "day9"
  "day10"
  "day11"
  "day12"
)

type Day interface {
	Run1() string
	Run2() string
}

type Conf struct {
	Name string
	Day Day
}

func main() {
	days := []Conf{
      Conf {"Day 1", day1.Day{} },
      Conf {"Day 2", day2.Day{} },
      Conf {"Day 3", day3.Day{} },
      Conf {"Day 4", day4.Day{} },
      Conf {"Day 5", day5.Day{} },
      Conf {"Day 6", day6.Day{} },
      Conf {"Day 7", day7.Day{} },
      Conf {"Day 8", day8.Day{} },
      Conf {"Day 9", day9.Day{} },
      Conf {"Day 10", day10.Day{} },
      Conf {"Day 11", day11.Day{} },
      Conf {"Day 12", day12.Day{} },
	}
	for _, d := range days {
		fmt.Println(d.Name)
		fmt.Println(d.Day.Run1());
		fmt.Println(d.Day.Run2());
	}
}
