package day7

import (
	"fmt"
	"helpers"
	"regexp"
	"strconv"
	"strings"
)

type Day struct {}

type BagContent struct{
	num int
	bagType string
}

var Empty struct{}

var filename = "/home/wampie/git/adventofcode/2020/inputs/day7"

// var ruleRE regexp = regexp.MustCompile(`(.+) bags contain (.+).`);
var bagAmountRE = regexp.MustCompile(`(\d+) ([^,]+) bag`);

func (d Day) Run1() string {
	var parsedinput = helpers.GetListOfStrings(filename)
	return fmt.Sprint(part1(parsedinput))
}

func (d Day) Run2() string {
	var parsedinput = helpers.GetListOfStrings(filename)
	return fmt.Sprint(part2(parsedinput))
}

func part1(rules []string) int {
	chart := parse1(rules)

	count := 0
	stack := []string {"shiny gold"}
	seen := make(map[string]struct{}, len(rules))

	for len(stack) > 0 {
		n := len(stack) - 1 // Top element
		current := stack[n]
		stack = stack[:n] // Pop

		for _, v := range chart[current] {
			if _, ok := seen[v]; !ok {
				stack = append(stack, v)
				seen[v] = Empty
				count++
			}
		}
	}

	
	return count
}

func parse1(rules []string) map[string][]string {
	chart := make(map[string][]string)

	for _, rule := range rules {
		contents, container := parse_rule(rule)
		for _, c := range contents {
			if _, ok := chart[c.bagType]; !ok {
				chart[c.bagType] = []string{ container }
				continue
			}
			chart[c.bagType] = append(chart[c.bagType], container)
		}
	}
	return chart
}

func parse_rule(rule string) (content []BagContent, key string) {
	split := strings.Split(rule, " bags contain ")
	key = split[0]
	matches := bagAmountRE.FindAllStringSubmatch(split[1], -1)

	content = make([]BagContent, len(matches))

	for i, v := range matches {
		num, _ := strconv.Atoi(v[1])
		content[i] = BagContent { num, v[2] }
	}
	return
}

type stackElem struct {
	amount int
	name string
}

func part2(rules []string) int {
	chart := parse2(rules)

	stack := []stackElem { {0, "shiny gold"} }
	worth := make(map[string]int, len(rules))

	for len(stack) > 0 {
		n := len(stack) - 1 // Top element
		current := stack[n]
		
		if len(chart[current.name]) != 0 {
			x := len(chart[current.name]) - 1
			next := chart[current.name][x]
			if _, ok := worth[next.bagType]; ok {
				worth[current.name] += (worth[next.bagType]+1)*next.num
				chart[current.name] = chart[current.name][:x]
				continue
			} else if len(chart[next.bagType]) == 0 {
				worth[current.name] += next.num
				chart[current.name] = chart[current.name][:x]
				continue
			} else {
				stack = append(stack, stackElem{next.num, next.bagType})
				continue
			}
		}
		stack = stack[:n] // Pop
	}
	
	return worth["shiny gold"];
}


func parse2(rules []string) map[string][]BagContent {
	chart := make(map[string][]BagContent)

	for _, rule := range rules {
		contents, container := parse_rule(rule)
		if _, ok := chart[container]; !ok {
			chart[container] = contents
			continue
		}
		chart[container] = append(chart[container], contents...)
	}
	return chart
}

