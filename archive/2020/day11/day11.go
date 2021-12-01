package day11

import (
	"fmt"
	"helpers"
)

type Day struct {}

var filename = "/home/wampie/git/adventofcode/2020/inputs/day11"

func (d Day) Run1() string {
	var parsedinput = helpers.GetListOfStrings(filename)
	return fmt.Sprint(part1(parsedinput))
}

func (d Day) Run2() string {
	var parsedinput = helpers.GetListOfStrings(filename)
	return fmt.Sprint(part2(parsedinput))
}

func part1(arr []string) int {
	area := parse(arr)

	for area.Step() {
		// next
	}

	return area.CountOccupied();
}

func part2(arr []string) int {
	area := parse(arr)

	area.part2 = true

	for area.Step() {
		// next
	}

	return area.CountOccupied();
}


func parse(arr []string) Area {
	prevStateHashes := make(StateSet)
	prevStateHashes[""] = Empty
	a := make([][]Tile, len(arr))
	b := make([][]Tile, len(arr))

	for i, y := range arr {
		a[i] = make([]Tile, len(y))
		b[i] = make([]Tile, len(y))
		for j, x := range y {
			a[i][j] = CreateTile(x)
			b[i][j] = CreateTile(x)
		}
	}
	return Area{
		prevStateHashes,
		a,
		b,
		0,
		false,
	}
}