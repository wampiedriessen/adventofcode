package day12

import (
	"fmt"
	"helpers"
	"math"
	"math/cmplx"
	"strconv"
)

type Day struct {}

var filename = "/home/wampie/git/adventofcode/2020/inputs/day12"

func (d Day) Run1() string {
	var parsedinput = helpers.GetListOfStrings(filename)
	return fmt.Sprint(part1(parsedinput))
}

func (d Day) Run2() string {
	var parsedinput = helpers.GetListOfStrings(filename)
	return fmt.Sprint(part2(parsedinput))
}

type Heading complex128

const (
	North Heading = 0 + 1i
	East Heading = 1 + 0i
	South Heading = 0 + -1i
	West Heading = -1 + 0i
	Left Heading = 99 + 0i
	Right Heading = 0 + 99i
	Forward Heading = 99 + 99i
)

func GetCommand(r byte) Heading {
	switch r {
	case 'N':
		return North
	case 'S':
		return South
	case 'E':
		return East
	case 'W':
		return West
	case 'L':
		return Left
	case 'R':
		return Right
	case 'F':
		return Forward
	}
	return Forward
}

type Instruction struct{
	op Heading
	num int
}

func parse(arr []string) []Instruction {
	instructions := make([]Instruction, len(arr))

	for i, v := range arr {
		instructions[i] = parseInstr(v)
	}
	return instructions
}

func parseInstr(cmd string) Instruction {
	num, _ := strconv.Atoi(cmd[1:])
	return Instruction {
		GetCommand(cmd[0]),
		num,
	}
}

func part1(arr []string) int {
	hdg := East
	pos := Heading(0 + 0i)

	instructions := parse(arr)

	for _, cmd := range instructions {
		hdg, pos = updateShip(cmd, hdg, pos)
	}

	return int(math.Abs(real(pos)) + math.Abs(imag(pos)))
}

func (c Heading) Round() Heading {
	r := math.Round(real(c))
	i := math.Round(imag(c))
	return Heading(complex(r, i))
}

func (h Heading) Multiply(c int) Heading {
	r, th := cmplx.Polar(complex128(h))
	return Heading(cmplx.Rect(r*float64(c), th))
}

func (h Heading) Rotate(c int) Heading {
	r, th := cmplx.Polar(complex128(h))

	th -= float64(c) / 180 * math.Pi

	return Heading(cmplx.Rect(r, th)).Round()
}

func updateShip(cmd Instruction, hdg Heading, pos Heading) (Heading, Heading) {
	switch cmd.op {
		case Forward:
			pos += hdg.Multiply(cmd.num)
		case Left:
			hdg = hdg.Rotate(-cmd.num)
		case Right:
			hdg = hdg.Rotate(cmd.num)
		default:
			pos += cmd.op.Multiply(cmd.num)
	}
	pos = pos.Round()
	return hdg, pos
}

func updateWaypoint(cmd Instruction, wayp Heading) Heading {
	switch cmd.op {
		case Left:
			wayp = wayp.Rotate(-cmd.num)
		case Right:
			wayp = wayp.Rotate(cmd.num)
		default:
			wayp += cmd.op.Multiply(cmd.num)
	}
	wayp = wayp.Round()
	return wayp
}

func part2(arr []string) int {
	waypoint := Heading(10 + 1i)
	pos := Heading(0 + 0i)

	instructions := parse(arr)

	for _, cmd := range instructions {
		if cmd.op == Forward {
			pos += waypoint.Multiply(cmd.num)
			pos = pos.Round()
			continue
		}
		waypoint = updateWaypoint(cmd, waypoint)
	}

	return int(math.Abs(real(pos)) + math.Abs(imag(pos)))
}

