package day5

import (
	"fmt"
	"helpers"
	"sort"
	"strconv"
	"strings"
)

type Day struct {}

var filename = "/home/wampie/git/adventofcode/2020/inputs/day5"

func (d Day) Run1() string {
	var parsedinput = helpers.GetListOfStrings(filename)
	return fmt.Sprint(part1(parsedinput))
}

func (d Day) Run2() string {
	var parsedinput = helpers.GetListOfStrings(filename)
	return fmt.Sprint(part2(parsedinput))
}

func SeatId(in string) int {
	in = strings.ReplaceAll(in, "F", "0");
	in = strings.ReplaceAll(in, "L", "0");
	in = strings.ReplaceAll(in, "B", "1");
	in = strings.ReplaceAll(in, "R", "1");

	seatId, _ := strconv.ParseInt(in, 2, 32)
	return int(seatId)
}

func part1(arr []string) int {
	var m int
	for i, e := range arr {
		s := SeatId(e)
		if i==0 || s > m {
			m = s
		}
	}
	return m;
}

func part2(arr []string) int {
	seats := make([]int, len(arr))
	for i, e := range arr {
		seats[i] = SeatId(e)
	}

	sort.Ints(seats)

	l := seats[0]
	for _, s := range seats[1:] {
		if (l+1) != s {
			return l+1
		}
		l = s
	}

	return 0;
}
