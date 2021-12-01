package day9

import (
	"fmt"
	"helpers"
)

type Day struct {}

var filename = "/home/wampie/git/adventofcode/2020/inputs/day9"

func (d Day) Run1() string {
	var parsedinput = helpers.GetListOfInts(filename)
	return fmt.Sprint(part1(parsedinput, 25))
}

func (d Day) Run2() string {
	var parsedinput = helpers.GetListOfInts(filename)
	return fmt.Sprint(part2(parsedinput, 25))
}

func part1(arr []int, preamble int) int {
	for x := preamble; x < len(arr); x++ {
		target := arr[x]

		found := false

		for i := x - preamble; i < x; i++ {
			for j := i+1; j < x; j++ {
				if arr[i] + arr[j] == target {
					found = true
					break
				}
			}
			if found {
				break;
			}
		}
		if !found {
			return target
		}
	}
	return 0;
}

func part2(arr []int, preamble int) int {
	target := part1(arr, preamble)

	a := 0
	b := 1

	for {
		sum := sumOff(arr[a:b])
		switch {
			case sum == target:
				return biggests(arr[a:b])
			case sum < target:
				b++
			case sum > target:
				a++
		}
	}
}

func sumOff(arr []int) int {
	sum := 0
	for _, x := range arr {
		sum += x
	}
	return sum
}

func biggests(arr []int) int {
	min := 999999999
	max := -999999999

	for _, x := range arr {
		if x > max {
			max = x
		}
		if x < min {
			min = x
		}
	}
	return max + min
}