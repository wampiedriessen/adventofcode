package day13

import (
	"testing"
)

func Test1(t *testing.T) {
	input := []string {
		"939",
		"7,13,x,x,59,x,31,19",
	}

	var output = part1(input);

	if output != 295 {
		t.Errorf("Not correct!: %v", output);
	}
}

var schedules = []struct {
	in string
	out int
}{
	// {"3,x,x,4,5", 39},
    {"7,13,x,x,59,x,31,19", 1068781},
    {"17,x,13,19", 3417},
    {"67,7,59,61", 754018},
    {"67,x,7,59,61", 779210},
    {"1789,37,47,1889", 1202161486},
}

func Test2(t *testing.T) {
	for _, tt := range schedules {
		t.Run(tt.in, func(t *testing.T) {
			s := part2(tt.in)
			if s != tt.out {
				t.Errorf("got %v, want %v", s, tt.out)
			}
		})
	}
}

func TestPart1(t *testing.T) {
	d := Day{}
	val := d.Run1()
	t.Log(val)
	if val != "102" {
		t.Error("Regression on p1")
	}
}

func TestPart2(t *testing.T) {
	d := Day{}
	val := d.Run2 ()
	t.Log(val)
	if val != "327300950120029" {
		t.Error("Regression on p2")
	}
}
