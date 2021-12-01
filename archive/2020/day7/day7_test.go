package day7

import (
	"testing"
	"helpers"
)

func Test1(t *testing.T) {
	var input = helpers.GetListOfStrings("example1");

	var output = part1(input);

	if output != 4 {
		t.Errorf("Not correct!: %v", output);
	}
}

func Test2(t *testing.T) {
	var input1 = helpers.GetListOfStrings("example1");
	var output1 = part2(input1);

	if output1 != 32 {
		t.Errorf("Not correct!: %v", output1);
	}

	var input2 = helpers.GetListOfStrings("example2");
	var output2 = part2(input2);

	if output2 != 126 {
		t.Errorf("Not correct!: %v", output2);
	}
}

func TestPart1(t *testing.T) {
	d := Day{}
	val := d.Run1()
	t.Log(val)
	if val != "119" {
		t.Error("Regression on p1")
	}
}

func TestPart2(t *testing.T) {
	d := Day{}
	val := d.Run2 ()
	t.Log(val)
	if val != "155802" {
		t.Error("Regression on p2")
	}
}
