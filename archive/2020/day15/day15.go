package day15

import (
	"fmt"
	"helpers"
)

type Day struct {}

var filename = "/home/wampie/git/adventofcode/2020/inputs/day15"

func (d Day) Run1() string {
	var parsedinput = helpers.GetListOfStrings(filename)[0]
	return fmt.Sprint(part1(parsedinput))
}

func (d Day) Run2() string {
	var parsedinput = helpers.GetListOfStrings(filename)[0]
	return fmt.Sprint(part2(parsedinput))
}

func part1(input string) int {
	return bruteForce(input, 2020)
}

func part2(input string) int {
	return bruteForce(input, 30000000)
}

func bruteForce(input string, count int) int {
	startInts := helpers.SplitStringToInts(input, ",");
	age := make(map[int]int, count)
	
	for i, v := range startInts {
		age[v] = i + 1
	}
	
	i := len(startInts)
	nextInt := 0

	for i < count - 1 {
		ageBefore, seenBefore := age[nextInt]
		age[nextInt] = i + 1
		if seenBefore {
			nextInt = i + 1 - ageBefore
		} else {
			nextInt = 0
		}
		i++
	}

	return nextInt;
}