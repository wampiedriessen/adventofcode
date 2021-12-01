package day1

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

func TestPart1(t *testing.T) {
	d := Day{}
	val := d.Run1()
	t.Log(val)
	if val != "1015476" {
		t.Error("Regression on Day1 p1")
	}
}

func TestPart2(t *testing.T) {
	d := Day{}
	val := d.Run2 ()
	t.Log(val)
	if val != "200878544" {
		t.Error("Regression on Day1 p2")
	}
}