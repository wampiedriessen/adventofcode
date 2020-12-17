package day11

import (
	"fmt"
)

var Empty struct{}

type StateSet map[string]struct {}

type Area struct {
	prevStateHashes StateSet
	state1 [][]Tile
	state2 [][]Tile
	current int
	part2 bool
}

type Tile byte

const (
    Seat Tile = iota
    Occupied
    Floor
)

func (d Tile) String() string {
    return [...]string{"L", "#", "."}[d]
}

func (d Area) String() string {
	result := fmt.Sprintln(d.GetStateHash())
	for _, y := range d.State() {
		for _, x := range y {
			result += x.String()
		}
		result += "\n"
	}
    return result
}

func (d *Area) State() [][]Tile {
	if d.current == 0 {
		return d.state1
	}
	if d.current == 1 {
		return d.state2
	}
	return nil
}

func (d *Area) PrevState() [][]Tile {
	if d.current == 1 {
		return d.state1
	}
	if d.current == 0 {
		return d.state2
	}
	return nil
}

func (d *Area) NextState() {
	d.current++
	d.current = d.current % 2
}

func (a *Area) GetStateHash() string {
	var sum string = ""
	for _, y := range a.State() {
		for _, x := range y {
			if x == Floor {
				continue
			}
			sum += x.String()
		}
	}
	return sum
}

func CreateTile(s rune) Tile {
	switch s {
	case 'L':
		return Seat
	case '.':
		return Floor
	case '#':
		return Occupied
	}
	return 0
}

func (a *Area) Step() bool {
	a.NextState()

	pState := a.PrevState()

	for i, y := range pState {
		for j, x := range y {
			count := getCount(pState, i, j)
			a.State()[i][j] = newState(x, count, a.GetTolerance())
		}
	}

	state := a.GetStateHash()
	if _, ok := a.prevStateHashes[state]; ok {
		return false
	}

	a.prevStateHashes[state] = Empty
	return true
}

func getCount(state [][]Tile, y int, x int) int {
	checkX := []int{ x }
	checkY := []int{ y }
	count := 0
	if x != 0 {
		checkX = append(checkX, x-1)
	}
	if x != len(state[0])-1 {
		checkX = append(checkX, x+1)
	}
	if y != 0 {
		checkY = append(checkY, y-1)
	}
	if y != len(state)-1 {
		checkY = append(checkY, y+1)
	}
	for _, v := range checkX {
		for _, w := range checkY {
			if !(v == x && w ==y) && state[w][v] == Occupied{
				count++
			}
		}
	}
	return count
}

func (a *Area) CountOccupied() int {
	count := 0
	for _, y := range a.State() {
		for _, x := range y {
			if x == Occupied {
				count++
			}
		}
	}
	return count
}

func (a *Area) GetTolerance() int {
	if a.part2 {
		return 5
	}
	return 4
}

func newState(t Tile, count int, tolerance int) Tile {
	if t == Seat && count == 0 {
		return Occupied
	}
	if t == Occupied && count >= tolerance {
		return Seat
	}
	return t
}