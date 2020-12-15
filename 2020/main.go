package main

import (
	"day2"
	"day1"
	"fmt"
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
		Conf {"Day 5", day5.Day{} },
		Conf {"Day 4", day4.Day{} },
		Conf {"Day 3", day3.Day{} },
		Conf {"Day 2", day2.Day{} },
		Conf {"Day 1", day1.Day{} },
	}
	for _, d := range days {
		fmt.Println(d.Name)
		fmt.Println(d.Day.Run1());
		fmt.Println(d.Day.Run2());
	}
}