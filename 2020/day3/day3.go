package day3

import (
	"fmt"
	"helpers"
)

type Day struct {}

var filename = "/home/wampie/git/adventofcode/2020/inputs/day3"

func (d Day) Run1() string {
	var parsedinput = helpers.GetListOfStrings(filename)
	return fmt.Sprint(part1(parsedinput))
}

func (d Day) Run2() string {
	var parsedinput = helpers.GetListOfStrings(filename)
	return fmt.Sprint(part2(parsedinput))
}

func part1(arr []string) int {
	return slope(arr, 3, 1);
}

func slope(arr []string, dR int, dD int) int {
	trees := 0
	r := 0
	for d := 0; d < len(arr); d += dD {
		if arr[d][r] == '#' {
			trees++
		}
		r = (r + dR) % len(arr[d])
	}
	return trees;
}

func part2(arr []string) int {
	return slope(arr, 1, 1)*
		slope(arr, 3, 1)*
		slope(arr, 5, 1)*
		slope(arr, 7, 1)*
		slope(arr, 1, 2);
}
