package day13

import (
	"fmt"
	"helpers"
	// "sort"
	"strconv"
	"strings"
)

type Day struct {}

var filename = "/home/wampie/git/adventofcode/2020/inputs/day13"

func (d Day) Run1() string {
	var parsedinput = helpers.GetListOfStrings(filename)
	return fmt.Sprint(part1(parsedinput))
}

func (d Day) Run2() string {
	var parsedinput = helpers.GetListOfStrings(filename)
	return fmt.Sprint(part2(parsedinput[1]))
}

func part1(arr []string) int {
	now, _ := strconv.Atoi(arr[0])

	minId := 0
	minTime := 9999999999

	for _, x := range strings.Split(arr[1], ",") {
		if (x == "x" || x == "") {
			continue
		}
		id, _ := strconv.Atoi(x)

		if now % id == 0 {
			return 0
		}
		time := id - (now % id)

		if time < minTime {
			minId = id
			minTime = time
		}
	}
	return minTime*minId;
}

type Mod struct {
	time int
	mod int
}

func part2(arr string) int {
	l := len(arr)
	nums := make([]Mod, l)
	found := 0
	multiple := 1

	for i, x := range strings.Split(arr, ",") {
		if x == "x" {
			continue
		}
		mod, _ := strconv.Atoi(x)
		nums[found] = Mod{ mod: mod, time: (mod - (i % mod)) % mod}
		multiple *= mod
		found++
	}

	nums = nums[:found]
	sum := 0

	for _, m := range nums {
		n := multiple / m.mod
		_, y := egcd(m.mod, n)

		sum += (y * n * m.time)
	}

	return positive_mod(sum, multiple)
}

func egcd(a int, b int) (int, int) {
	if b == 0 {
		return 1, 0
	}
	q, r := a/b, a%b
	x, y := egcd(b, r)
	return y, x - q * y
}

func positive_mod(a int, b int) int {
	m := a % b

	if m < 0 {
		return m + b
	}

	return m
}