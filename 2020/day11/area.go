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
			var count int
			if a.part2 {
				count = getSeen(pState, i, j)
			} else {
				count = getCount(pState, i, j)
			}
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

type Coord struct{
	x int
	y int
}

var directions = []Coord{
	{-1, -1},
	{-1, 0},
	{-1, 1},
	{0, -1},
	{0, 1},
	{1, -1},
	{1, 0},
	{1, 1},
}

type LookingStruct struct{
	x int
	y int
	solved bool
}

func getSeen(state [][]Tile, y int, x int) int {
	count := 0
	solved := 0
	var looking = make([]LookingStruct, 8)

	for i, dir := range directions {
		looking[i] = LookingStruct{dir.x, dir.y, false}
	}

	for solved != 8 {
		for i, dir := range looking {
			if dir.solved {
				continue
			}
			nx := x + dir.x
			ny := y + dir.y
			if ny < 0 || nx < 0 || ny >= len(state) || nx >= len(state[ny]) {
				looking[i].solved = true
				solved++
				continue
			}
			if state[ny][nx] != Floor {
				looking[i].solved = true
				solved++
			}
			if state[ny][nx] == Occupied {
				count++
			}
			looking[i].x += directions[i].x
			looking[i].y += directions[i].y
		}
	}
	return count
}

func getCount(state [][]Tile, y int, x int) int {
	count := 0
	for _, dir := range directions {
		nx := x + dir.x
		ny := y + dir.y
		if ny < 0 || nx < 0 || ny >= len(state) || nx >= len(state[ny]) {
			continue
		}
		if state[ny][nx] == Occupied {
			count++
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