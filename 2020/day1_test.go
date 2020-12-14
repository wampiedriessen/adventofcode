package main

import "testing"

func Test1(t *testing.T) {
	var input = []int{1721, 979, 366, 299, 675, 1456};

	var output = part1(input, 2020);

	if output != 514579 {
		t.Errorf("Not correct!: %v", output);
	}
}

func Test2(t *testing.T) {
	var input = []int{1721, 979, 366, 299, 675, 1456};

	var output = part2(input);

	if output != 241861950 {
		t.Errorf("Not correct!: %v", output);
	}
}

func Part1(t *testing.T) {
	var parsedinput = parse()
	if part1(parsedinput, 2020) != 1015476 {
		t.Error("Regression on Day1 p1")
	}
}

func Part2(t *testing.T) {
	var parsedinput = parse()
	if part2(parsedinput) != 200878544 {
		t.Error("Regression on Day1 p2")
	}
}