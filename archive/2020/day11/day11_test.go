package day11

import (
	"helpers"
	"testing"
)

var example1 = helpers.GetListOfStrings("example1")

func Test1(t *testing.T) {
	var output = part1(example1);
	
	if output != 37 {
		t.Errorf("Not correct!: %v", output);
	}
}

func Test2(t *testing.T) {
	var output = part2(example1);

	if output != 26 {
		t.Errorf("Not correct!: %v", output);
	}
}

func TestPart1(t *testing.T) {
	d := Day{}
	val := d.Run1()
	t.Log(val)
	if val != "2468" {
		t.Error("Regression on p1")
	}
}

func TestPart2(t *testing.T) {
	d := Day{}
	val := d.Run2 ()
	t.Log(val)
	if val != "2214" {
		t.Error("Regression on p2")
	}
}
