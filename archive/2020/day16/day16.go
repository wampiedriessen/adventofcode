package day16

import (
	"fmt"
	"helpers"
	"regexp"
	"strconv"
)

type Day struct {}

var filename = "/home/wampie/git/adventofcode/2020/inputs/day16"

type NumRange struct {
	low int
	high int
}

type TicketFieldRule struct {
	name string
	range1 NumRange
	range2 NumRange
}

func (rule *TicketFieldRule) IsValidValue(val int) bool {
	return rule.range1.low <= val && rule.range1.high >= val || 
			rule.range2.low <= val && rule.range2.high >= val;
}

type Ticket []int

type Input struct {
	TicketFieldRules []TicketFieldRule
	MyTicket Ticket
	NearbyTickets []Ticket
}

func (d Day) Run1() string {
	var parsedinput = helpers.GetListOfStrings(filename)
	return fmt.Sprint(part1(parsedinput))
}

func (d Day) Run2() string {
	var parsedinput = helpers.GetListOfStrings(filename)
	return fmt.Sprint(part2(parsedinput, "departure"))
}

var ticketRuleRE = regexp.MustCompile(`(.*): ([0-9]+)-([0-9]+) or ([0-9]+)-([0-9]+)`)

func parseRule(rule string) TicketFieldRule {
	matches := ticketRuleRE.FindStringSubmatch(rule)

	a, _ := strconv.Atoi(matches[2])
	b, _ := strconv.Atoi(matches[3])
	c, _ := strconv.Atoi(matches[4])
	d, _ := strconv.Atoi(matches[5])

	return TicketFieldRule {
		matches[1],
		NumRange {
			a,
			b,
		},
		NumRange {
			c,
			d,
		},
	}
}

func parseTicket(arr string) Ticket {
	nums := helpers.SplitStringToInts(arr, ",")
	return Ticket(nums)
}

func parse(arr []string) Input {
	rules := make([]TicketFieldRule, 0)
	
	i := 0
	for arr[i] != "" {
		rules = append(rules, parseRule(arr[i]))
		i++
	}
	i += 2
	yourTicket := parseTicket(arr[i])
	i += 2
	tickets := make([]Ticket, 0)
	for _, x := range arr[i+1:] {
		tickets = append(tickets, parseTicket(x))
	}

	return Input{
		rules,
		yourTicket,
		tickets,
	}
}

func scanTickets(input *Input) (int, []Ticket) {
	sum := 0
	validTickets := make([]Ticket, 0)

	for _, t := range input.NearbyTickets {
		ticketValid := true
		for _, tv := range t {
			valid := false
			for _, rule := range input.TicketFieldRules {
				if rule.IsValidValue(tv) {
					valid = true
					break;
				}
			}
			if !valid {
				ticketValid = false
				sum += tv
			}
		}
		if ticketValid {
			validTickets = append(validTickets, t)
		}
	}

	return sum, validTickets
}

func part1(arr []string) int {
	input := parse(arr);

	sum, valid := scanTickets(&input)

	fmt.Println(len(valid))

	return sum;
}

var Empty struct{}

func part2(arr []string, startsWith string) int {
	input := parse(arr);

	_, validTickets := scanTickets(&input)

	multiple := 1

	possibilities := make(map[int]map[int]struct{}, len(input.MyTicket))

	for i, _ := range input.TicketFieldRules {
		if _, ok := possibilities[i]; !ok {
			possibilities[i] = make(map[int]struct{}, len(input.MyTicket))
			for j, _ := range input.MyTicket {
				possibilities[i][j] = Empty
			}
		}
	}

	for _, t := range validTickets {
		for i, rule := range input.TicketFieldRules {
			for j, tv := range t {
				if _, ok := possibilities[i][j]; ok && !rule.IsValidValue(tv) {
					delete(possibilities[i], j);
				}
			}
		}
	}

	fmt.Println(possibilities)

	return multiple;
}
