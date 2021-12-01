package day16

import (
	"helpers"
	"testing"
)

func Test1(t *testing.T) {
	var input = helpers.GetListOfStrings("example1")

	var output = part1(input);

	if output != 71 {
		t.Errorf("Not correct!: %v", output);
	}
}

func Test2(t *testing.T) {
	var input = helpers.GetListOfStrings("example2")

	var output = part2(input, "class");

	if output != 12 {
		t.Errorf("Not correct!: %v", output);
	}
}

func TestPart1(t *testing.T) {
	d := Day{}
	val := d.Run1()
	t.Log(val)
	if val != "0" {
		t.Error("Regression on p1")
	}
}

func TestPart2(t *testing.T) {
	d := Day{}
	val := d.Run2 ()
	t.Log(val)
	if val != "0" {
		t.Error("Regression on p2")
	}
}
