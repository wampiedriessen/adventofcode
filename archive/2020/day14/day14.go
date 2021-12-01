package day14

import (
	"fmt"
	"helpers"
	"regexp"
	"strconv"
)

type Day struct {}

var filename = "/home/wampie/git/adventofcode/2020/inputs/day14"

func (d Day) Run1() string {
	var parsedinput = helpers.GetListOfStrings(filename)
	return fmt.Sprint(part1(parsedinput))
}

func (d Day) Run2() string {
	var parsedinput = helpers.GetListOfStrings(filename)
	return fmt.Sprint(part2(parsedinput))
}

type Bitmap struct{
	bit rune
	pos int
}

var maskInstructionRE = regexp.MustCompile(`mask = ([X01]{36})`)
var memInstructionRE = regexp.MustCompile(`mem\[([0-9]+)\] = ([0-9]+)`)

func part1(arr []string) int {
	mem := make(map[int]int, len(arr))

	var bitmask []Bitmap
	
	for _, x := range arr {
		memMatch := memInstructionRE.FindStringSubmatch(x)
		if len(memMatch) == 3 {
			mloc, _ := strconv.Atoi(memMatch[1])
			val, _ := strconv.Atoi(memMatch[2])
			mem[mloc] = applyBitmask(bitmask, val, false)[0]
		} else {
			maskMatch := maskInstructionRE.FindStringSubmatch(x)
			bitmask = createMask(maskMatch[1])
		}
		// fmt.Println(mem)
	}

	sum := 0

	for _, v := range mem {
		sum += v
	}

	return sum;
}

func createMask(mask string) []Bitmap {
	bitmap := make([]Bitmap, 0)
	for i, c := range mask {
		bitmap = append(bitmap, Bitmap{c, i})
	}
	return bitmap
}

func applyBitmask(bm []Bitmap, inVal int, part2 bool) []int {
	vals := []int { inVal }
	for _, x := range bm {
		for i, val := range vals {
			if x.bit == '1' {
				vals[i] = mask1(val, x.pos)
			}
			if !part2 && x.bit == '0' {
				vals[i] = mask0(val, x.pos)
			}
			if part2 && x.bit == 'X'{
				vals[i] = mask0(val, x.pos)
				vals = append(vals, mask1(val, x.pos))
			}
		}
	}
	return vals
}

func mask1(val int, pos int) int {
	return val | (1 << (35-pos))
}

func mask0(val int, pos int) int {
	return val & (((1 << 36) - 1) - (1 << (35-pos)))
}

func part2(arr []string) int {
	mem := make(map[int]int, len(arr))

	var bitmask []Bitmap
	
	for _, x := range arr {
		memMatch := memInstructionRE.FindStringSubmatch(x)
		if len(memMatch) == 3 {
			mloc, _ := strconv.Atoi(memMatch[1])
			val, _ := strconv.Atoi(memMatch[2])

			mems := applyBitmask(bitmask, mloc, true)
			for _, m := range mems {
				mem[m] = val
			}
		} else {
			maskMatch := maskInstructionRE.FindStringSubmatch(x)
			bitmask = createMask(maskMatch[1])
		}
	}

	sum := 0

	for _, v := range mem {
		sum += v
	}

	return sum;
}
