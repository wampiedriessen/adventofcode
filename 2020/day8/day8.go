package day8

import (
	"fmt"
	"helpers"
	"regexp"
	"strconv"
)

type Day struct {}

var Empty struct{}

var filename = "/home/wampie/git/adventofcode/2020/inputs/day8"

func (d Day) Run1() string {
	var parsedinput = helpers.GetListOfStrings(filename)
	return fmt.Sprint(part1(parsedinput))
}

func (d Day) Run2() string {
	var parsedinput = helpers.GetListOfStrings(filename)
	return fmt.Sprint(part2(parsedinput))
}

var instructionRE = regexp.MustCompile(`([a-z]+) ([+\-]\d+)`)

func parse_instructions(input []string) []Instruction {
	result := make([]Instruction, len(input))
	for i, rule := range input {
		matches := instructionRE.FindStringSubmatch(rule)
		val, _ := strconv.Atoi(matches[2]);
		result[i] = Instruction {
			matches[1],
			val,
		}
	}
	return result
}

func part1(input []string) int {
	p := Program {
		0,
		0,
		parse_instructions(input),
	}

	been := make(map[int]struct{}, len(p.instructions))
	retraced := false
	lastAcc := p.acc

	for !retraced {
		lastAcc = p.acc
		been[p.pc] = Empty
		p.Step()
		_, retraced = been[p.pc]
	}
	return lastAcc;
}

func findLoop(p Program) bool {
	been := make(map[int]struct{}, len(p.instructions))
	retraced := false

	for !retraced && !p.IsFinished() {
		been[p.pc] = Empty
		p.Step()
		_, retraced = been[p.pc]
	}
	return p.IsFinished()
}

func part2(input []string) int {
	p := Program {
		0,
		0,
		parse_instructions(input),
	}

	for {
		if p.GetCurrentOp() == "nop" {
			p.ChangeCurrentOp("jmp")
			if findLoop(p) {
				break;
			}
			p.ChangeCurrentOp("nop")
		} else if p.GetCurrentOp() == "jmp" {
			p.ChangeCurrentOp("nop")
			if findLoop(p) {
				break;
			}
			p.ChangeCurrentOp("jmp")
		}
		p.Step()
	}
	p.Run()
	return p.acc;
}
