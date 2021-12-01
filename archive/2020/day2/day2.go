package day2

import (
	"fmt"
	"helpers"
	"regexp"
	"strconv"
	"strings"
)

type Day struct {}

var filename = "/home/wampie/git/adventofcode/2020/inputs/day2"

func (d Day) Run1() string {
	var parsedinput = helpers.GetListOfStrings(filename)
	return fmt.Sprint(part1(parsedinput))
}

func (d Day) Run2() string {
	var parsedinput = helpers.GetListOfStrings(filename)
	return fmt.Sprint(part2(parsedinput))
}

func part1(arr []string) int {
	count := 0;
	for _, v := range arr {
		if validate1(v) {
			count += 1
		}
	}
	return count;
}

func validate1(v string) bool {
	re := regexp.MustCompile(`(\d+)-(\d+) (.+): (.+)`)
	matches := re.FindStringSubmatch(v)
	min, _ := strconv.Atoi(matches[1])	
	max, _ := strconv.Atoi(matches[2])
	r := matches[3]
	passw := matches[4]

	c := strings.Count(passw, r)

	return c >= min && c <= max
}

func part2(arr []string) int {
	count := 0;
	for _, v := range arr {
		if validate2(v) {
			count += 1
		}
	}
	return count;
}

func validate2(v string) bool {
	re := regexp.MustCompile(`(\d+)-(\d+) (.+): (.+)`)
	matches := re.FindStringSubmatch(v)
	a, _ := strconv.Atoi(matches[1])	
	b, _ := strconv.Atoi(matches[2])
	r := matches[3]
	passw := matches[4]

	m1 := passw[a-1] == r[0];
	m2 := passw[b-1] == r[0];

	return m1 != m2
}