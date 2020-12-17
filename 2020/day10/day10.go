package day10

import (
	"fmt"
	"helpers"
	// "math"
	"sort"
)

type Day struct {}

var filename = "/home/wampie/git/adventofcode/2020/inputs/day10"

func (d Day) Run1() string {
	var parsedinput = helpers.GetListOfInts(filename)
	return fmt.Sprint(part1(parsedinput))
}

func (d Day) Run2() string {
	var parsedinput = helpers.GetListOfInts(filename)
	return fmt.Sprint(part2(parsedinput))
}

func part1(arr []int) int {
	sort.Ints(arr)

	v := 0
	a := 0
	b := 1

	for _, x := range arr {
		switch {
		case x - v == 1:
			a++
		case x - v == 3:
			b++
		}
		v = x
	}

	return a*b;
}

func part2(arr []int) int {
	arr = append(arr, 0)
	sort.Ints(arr)

	arr = append(arr, arr[len(arr)-1]+3)

	lastJump := 0
	possibilities := 1

	for i, x := range arr {
		if i != len(arr)-1 && arr[i+1] - x == 3 {
			possibilities *= poss(i - lastJump + 1)
			lastJump = i+1
		}
	}

	return possibilities;
}

func poss(num int) int {
	switch num {
		case 1:
			return 1
		case 2:
			return 1
		case 3:
			return 2
		case 4:
			return 4
		case 5:
			return 7
		case 6:
			return 13
		case 7:
			return 27
	}
	return -1
}