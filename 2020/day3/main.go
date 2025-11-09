package main

import (
	"common"
	"fmt"
	"math"
	"strconv"
	"strings"
)

type Direction int

const (
	Left Direction = iota
	Right
	Up
	Down
)

type WirePart struct {
	dir    Direction
	amount int
}

func parseWirePart(part string) WirePart {
	var ret WirePart
	switch part[0] {
	case 'L':
		ret.dir = Left
	case 'R':
		ret.dir = Right
	case 'U':
		ret.dir = Up
	case 'D':
		ret.dir = Down
	}
	amount, err := strconv.ParseInt(part[1:], 10, 64)
	if err != nil {
		panic(err)
	}
	ret.amount = int(amount)
	return ret
}

func parse(content string) ([]WirePart, []WirePart) {
	var wires [2]string
	i := 0
	for l := range strings.Lines(content) {
		wires[i] = strings.TrimSpace(l)
		i++
	}
	w1 := make([]WirePart, 0)
	for _, p := range strings.Split(wires[0], ",") {
		w1 = append(w1, parseWirePart(p))
	}
	w2 := make([]WirePart, 0)
	for _, p := range strings.Split(wires[1], ",") {
		w2 = append(w2, parseWirePart(p))
	}
	return w1, w2
}

func abs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}

func solvePart1(w1, w2 []WirePart) int {
	cache := make(map[string]bool)
	x := 0
	y := 0
	for _, p := range w1 {
		for i := 0; i < p.amount; i++ {
			switch p.dir {
			case Left:
				x--
			case Right:
				x++
			case Up:
				y++
			case Down:
				y--
			}
			key := fmt.Sprintf("%v#%v", x, y)
			cache[key] = true
		}
	}
	x = 0
	y = 0
	min := math.MaxInt
	for _, p := range w2 {
		for i := 0; i < p.amount; i++ {
			switch p.dir {
			case Left:
				x--
			case Right:
				x++
			case Up:
				y++
			case Down:
				y--
			}
			key := fmt.Sprintf("%v#%v", x, y)
			_, exists := cache[key]
			if dist := abs(x) + abs(y); exists && dist < min {
				min = dist
			}
		}
	}
	return min
}

func solvePart2(w1, w2 []WirePart) int {
	cache := make(map[string]int)
	x := 0
	y := 0
	s := 1
	for _, p := range w1 {
		for i := 0; i < p.amount; i++ {
			switch p.dir {
			case Left:
				x--
			case Right:
				x++
			case Up:
				y++
			case Down:
				y--
			}
			key := fmt.Sprintf("%v#%v", x, y)
			cache[key] = s
			s++
		}
	}
	x = 0
	y = 0
	s = 1
	min := math.MaxInt
	for _, p := range w2 {
		for i := 0; i < p.amount; i++ {
			switch p.dir {
			case Left:
				x--
			case Right:
				x++
			case Up:
				y++
			case Down:
				y--
			}
			key := fmt.Sprintf("%v#%v", x, y)
			steps, exists := cache[key]
			if total := steps + s; exists && total < min {
				min = total
			}
			s++
		}
	}
	return min
}

func main() {
	wire1, wire2 := parse(common.Read("./inputs/example.txt"))
	fmt.Printf("Part 1: %v\n", solvePart1(wire1, wire2))
	fmt.Printf("Part 2: %v\n", solvePart2(wire1, wire2))
}
