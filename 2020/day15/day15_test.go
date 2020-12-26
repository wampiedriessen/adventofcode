package day15

import "testing"

var examples = []struct {
	in string
	out1 int
	out2 int
}{
    {"0,3,6", 436, 175594}, 
    {"1,3,2", 1, 2578}, 
    {"2,1,3", 10, 3544142}, 
    {"1,2,3", 27, 261214}, 
    {"2,3,1", 78, 6895259}, 
    {"3,2,1", 438, 18}, 
    {"3,1,2", 1836, 362}, 
}

func Test1(t *testing.T) {
	for _, tt := range examples {
		t.Run(tt.in, func(t *testing.T) {
			s := part1(tt.in)
			if s != tt.out1 {
				t.Errorf("got %v, want %v", s, tt.out1)
			}
		})
	}
}

func Test2(t *testing.T) {
	for _, tt := range examples {
		t.Run(tt.in, func(t *testing.T) {
			s := part2(tt.in)
			if s != tt.out2 {
				t.Errorf("got %v, want %v", s, tt.out2)
			}
		})
	}
}

func TestPart1(t *testing.T) {
	d := Day{}
	val := d.Run1()
	t.Log(val)
	if val != "441" {
		t.Error("Regression on p1")
	}
}

func TestPart2(t *testing.T) {
	d := Day{}
	val := d.Run2 ()
	t.Log(val)
	if val != "10613991" {
		t.Error("Regression on p2")
	}
}
