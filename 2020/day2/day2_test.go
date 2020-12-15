package day2

import "testing"

var testinput = []string{
	"1-3 a: abcde",
	"1-3 b: cdefg",
	"2-9 c: ccccccccc",
	};

func Test1(t *testing.T) {
	var output = part1(testinput);

	if output != 2 {
		t.Errorf("Not correct!: %v", output);
	}
}

func Test2(t *testing.T) {
	var output = part2(testinput);

	if output != 1 {
		t.Errorf("Not correct!: %v", output);
	}
}

func TestPart1(t *testing.T) {
	d := Day{}
	val := d.Run1()
	t.Log(val)
	if val != 640 {
		t.Error("Regression on p1")
	}
}

func TestPart2(t *testing.T) {
	d := Day{}
	val := d.Run2()
	t.Log(val)
	if val != 472 {
		t.Error("Regression on p2")
	}
}