package day10

import "testing"

var example1 = []int{ 16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4 };
var example2 = []int{ 28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45, 19, 38, 39, 11, 1, 32, 25, 35, 8, 17, 7, 9, 4, 2, 34, 10, 3 };

func Test1(t *testing.T) {

	var output = part1(example1);

	if output != 35 {
		t.Errorf("Not correct!: %v", output);
	}

	output = part1(example2);

	if output != 220 {
		t.Errorf("Not correct!: %v", output);
	}
}

func Test2(t *testing.T) {
	var output = part2(example1);

	if output != 8 {
		t.Errorf("Not correct!: %v", output);
	}
	
	output = part2(example2);

	if output != 19208 {
		t.Errorf("Not correct!: %v", output);
	}
}

func TestPart1(t *testing.T) {
	d := Day{}
	val := d.Run1()
	t.Log(val)
	if val != "2664" {
		t.Error("Regression on p1")
	}
}

func TestPart2(t *testing.T) {
	d := Day{}
	val := d.Run2 ()
	t.Log(val)
	if val != "148098383347712" {
		t.Error("Regression on p2")
	}
}
