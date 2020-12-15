package day5

import "testing"

var tests = []struct {
	in string
	out int
}{
    {"BFFFBBFRRR", 567},
    {"FFFBBBFRRR", 119},
    {"BBFFBBFRLL", 820},
}

func TestSeatIds(t *testing.T) {
	for _, tt := range tests {
		t.Run(tt.in, func(t *testing.T) {
			s := SeatId(tt.in)
			if s != tt.out {
				t.Errorf("got %v, want %v", s, tt.out)
			}
		})
	}
}

func Test1(t *testing.T) {
	var input = make([]string, len(tests))

	for i, v := range tests {
		input[i] = v.in
	}

	var output = part1(input);

	var x int = 820;
	if output != x {
		t.Errorf("got %v, want %v", output, x)
	}
}

func TestPart1(t *testing.T) {
	d := Day{}
	val := d.Run1()
	t.Log(val)
	if val != "994" {
		t.Error("Regression on p1")
	}
}

func TestPart2(t *testing.T) {
	d := Day{}
	val := d.Run2 ()
	t.Log(val)
	if val != "741" {
		t.Error("Regression on p2")
	}
}
