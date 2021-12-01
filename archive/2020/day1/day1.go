package day1

import (
	"fmt"
	"helpers"
)

type Day struct {}

var filename = "/home/wampie/git/adventofcode/2020/inputs/day1"

func (d Day) Run1() string {
	var parsedinput = helpers.GetListOfInts(filename)
	return fmt.Sprint(part1(parsedinput, 2020))
}

func (d Day) Run2() string {
	var parsedinput = helpers.GetListOfInts(filename)
	return fmt.Sprint(part2(parsedinput))
}

func part1(arr []int, target int) int {
	var i, j int;
	for i = 0; i < len(arr); i++ {
		for j = (i+1); j < len(arr); j++ {
			if arr[j]+arr[i] == target {
				return arr[j]*arr[i];
			}
		}
	}
	return 0;
}

func part2(arr []int) int {
	var i int;
	for i = 0; i < len(arr)-1; i++ {
		val := part1(arr[i+1:], 2020 - arr[i])
		if val != 0 {
			return val * arr[i]
		}
	}
	return 0;
}
