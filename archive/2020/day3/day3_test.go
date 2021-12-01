package day3

import (
	"helpers"
	"testing"
)

func Test1(t *testing.T) {
	var input = helpers.GetListOfStrings("example1")

	var output = part1(input);

	if output != 7 {
		t.Errorf("Not correct!: %v", output);
	}
}

func Test2(t *testing.T) {
	var input = helpers.GetListOfStrings("example1")

	var output = part2(input);

	if output != 336 {
		t.Errorf("Not correct!: %v", output);
	}
}

func TestPart1(t *testing.T) {
	d := Day{}
	val := d.Run1()
	t.Log(val)
	if val != "234" {
		t.Error("Regression on p1")
	}
}

func TestPart2(t *testing.T) {
	d := Day{}
	val := d.Run2 ()
	t.Log(val)
	if val != "5813773056" {
		t.Error("Regression on p2")
	}
}
