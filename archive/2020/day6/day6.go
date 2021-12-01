package day6

import (
	"fmt"
	"helpers"
	"strings"
)

type Day struct {}

var filename = "/home/wampie/git/adventofcode/2020/inputs/day6"

var Empty struct{}

func (d Day) Run1() string {
	var parsedinput = helpers.GetListOfStrings(filename)
	return fmt.Sprint(part1(parsedinput))
}

func (d Day) Run2() string {
	var parsedinput = helpers.GetListOfStrings(filename)
	return fmt.Sprint(part2(parsedinput))
}

func part1(arr []string) int {
	sets := parse(arr)

	sum := 0

	for _, set := range sets {
		sum += len(set)
	}
	return sum;
}

func parse(arr []string) []map[rune]struct{} {
	var result []map[rune]struct{};

	begin := 0
	for i := 0; i < len(arr); i++ {
		if arr[i] == "" {
			result = append(result, parse_group(arr[begin:i]))
			begin = i + 1;
		}
	}
	result = append(result, parse_group(arr[begin:]))

	return result
}

func parse_group(arr []string) map[rune]struct{} {
	result := make(map[rune]struct{})

	for _, c := range strings.Join(arr, "") {
		result[c] = Empty;
	}

	return result
}

func part2(arr []string) int {
	sets := parse2(arr)

	sum := 0

	for _, set := range sets {
		sum += len(set)
	}
	return sum;
}

func parse2(arr []string) []map[rune]struct{} {
	var result []map[rune]struct{};

	begin := 0
	for i := 0; i < len(arr); i++ {
		if arr[i] == "" {
			result = append(result, parse_group2(arr[begin:i]))
			begin = i + 1;
		}
	}
	result = append(result, parse_group2(arr[begin:]))

	return result
}

func parse_group2(arr []string) map[rune]struct{} {
	keys := make(map[rune]struct{}, len(arr[0]))
	
	for _, c := range arr[0] {
		keys[c] = Empty;
	}
	
	for _, answs := range arr[1:] {
		for k := range keys {
			if !strings.Contains(answs, string(k)) {
				delete(keys, k);
			}
		}
	}

	return keys
}