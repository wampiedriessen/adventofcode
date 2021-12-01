package day4

import (
	"fmt"
	"helpers"
	"regexp"
	"strconv"
	"strings"
)

type Day struct {}

var filename = "/home/wampie/git/adventofcode/2020/inputs/day4"

var musthaves = []string {
    "byr",
    "iyr",
    "eyr",
    "hgt",
    "hcl",
    "ecl",
    "pid",
    // "cid",
}

func (d Day) Run1() string {
	var parsedinput = helpers.GetListOfStrings(filename)
	return fmt.Sprint(part1(parsedinput))
}

func (d Day) Run2() string {
	var parsedinput = helpers.GetListOfStrings(filename)
	return fmt.Sprint(part2(parsedinput))
}

func part1(arr []string) int {
	passports := parse(arr)
	count := 0

	for _, pp := range passports {
		all := true
		for _, mh := range musthaves {
			if _, ok := pp[mh]; !ok {
				all = false
			}
		}
		if all {
			count++
		}
	}
	return count;
}

func parse(arr []string) []map[string]string {
	var result []map[string]string;

	begin := 0
	for i := 0; i < len(arr); i++ {
		if arr[i] == "" {
			result = append(result, parse_passport(arr[begin:i]))
			begin = i + 1;
		}
	}
	result = append(result, parse_passport(arr[begin:]))

	return result
}

func parse_passport(arr []string) map[string]string {
	line := strings.Join(arr, " ");
	kvps := strings.Split(line, " ")

	result := make(map[string]string) 

	for _, v := range kvps {
		v2 := strings.Split(v, ":")
		result[v2[0]] = v2[1]
	}

	return result
}

func part2(arr []string) int {
	passports := parse(arr)
	count := 0

	for _, pp := range passports {
		all := true
		for _, mh := range musthaves {
			val, ok := pp[mh]
			if !ok {
				all = false
				break
			}
			if !validate(mh, val) {
				all = false
				break
			}
		}
		if all {
			count++
		}
	}
	return count;
}

var valid_ecl map[string]struct{} = map[string]struct {} {
	"amb": {},
	"blu": {},
	"brn": {},
	"gry": {},
	"grn": {},
	"hzl": {},
	"oth": {},
}

func validate(key string, val string) bool {
	switch key {
	case "byr":
		if ival, err := strconv.Atoi(val); err != nil || ival < 1920 || ival > 2002 {
			return false
		}
	case "iyr":
		if ival, err := strconv.Atoi(val); err != nil || ival < 2010 || ival > 2020 {
			return false
		}
	case "eyr":
		if ival, err := strconv.Atoi(val); err != nil || ival < 2020 || ival > 2030 {
			return false
		}
	case "hgt":
		if !valid_height(val) {
			return false
		}
	case "hcl":
		if m, _ := regexp.MatchString(`#[0-9a-f]{6}`, val); !m {
			return false
		}
	case "ecl":
		if _, ok := valid_ecl[val]; !ok {
			return false
		}
	case "pid":
		if _, err := strconv.Atoi(val); len(val) != 9 || err != nil {
			return false
		}
	}
	return true
}

func valid_height(h string) bool {
	ms := h[len(h)-2:]
	l := h[:len(h)-2]
	switch ms {
	case "in":
		if ival, err := strconv.Atoi(l); err != nil || ival < 59 || ival > 76 {
			return false
		}
	case "cm":
		if ival, err := strconv.Atoi(l); err != nil || ival < 150 || ival > 193 {
			return false
		}
	default:
		return false
	}
	return true
}