package main

import (
	"common"
	"fmt"
	"strings"
)

func parse(input string) [][]byte {
	ret := make([][]byte, 0)
	for line := range strings.Lines(input) {
		ret = append(ret, []byte(strings.TrimSpace(line)))
	}
	return ret
}

func encode(input [][]byte) int {
	key := 0
	current := 1
	for i := 0; i < len(input); i++ {
		for j := 0; j < len(input[i]); j++ {
			if input[i][j] == '#' {
				key += current
			}
			current *= 2
		}
	}
	return key
}

func simulate1(input [][]byte) int {
	inputCopy1 := make([][]byte, len(input))
	inputCopy2 := make([][]byte, len(input))
	for i := 0; i < len(inputCopy1); i++ {
		inputCopy1[i] = make([]byte, len(input[i]))
		copy(inputCopy1[i], input[i])
		inputCopy2[i] = make([]byte, len(input[i]))
	}
	layoutMap := make(map[int]bool)
	dirs := [4][2]int{
		{0, 1},
		{1, 0},
		{0, -1},
		{-1, 0},
	}
	for {
		key := encode(inputCopy1)
		if _, exists := layoutMap[key]; exists {
			return key
		}
		layoutMap[key] = true
		for i := 0; i < len(inputCopy1); i++ {
			for j := 0; j < len(inputCopy1[i]); j++ {
				neighbours := 0
				for _, dir := range dirs {
					ni := i + dir[0]
					nj := j + dir[1]
					if ni >= 0 && ni < len(inputCopy1) && nj >= 0 && nj < len(inputCopy1[ni]) && inputCopy1[ni][nj] == '#' {
						neighbours++
					}
				}
				if inputCopy1[i][j] == '#' && neighbours != 1 {
					inputCopy2[i][j] = '.'
				} else if inputCopy1[i][j] == '.' && (neighbours == 1 || neighbours == 2) {
					inputCopy2[i][j] = '#'
				} else {
					inputCopy2[i][j] = inputCopy1[i][j]
				}
			}
		}
		tmp := inputCopy2
		inputCopy2 = inputCopy1
		inputCopy1 = tmp
	}
}

func getNeighbours(l, i, j int) [][3]int {
	ret := make([][3]int, 0)
	if i == 1 && j == 2 {
		return [][3]int{
			{l, 0, 2},
			{l, 1, 1},
			{l, 1, 3},
			{l + 1, 0, 0},
			{l + 1, 0, 1},
			{l + 1, 0, 2},
			{l + 1, 0, 3},
			{l + 1, 0, 4},
		}
	}
	if i == 2 && j == 1 {
		return [][3]int{
			{l, 1, 1},
			{l, 2, 0},
			{l, 3, 1},
			{l + 1, 0, 0},
			{l + 1, 1, 0},
			{l + 1, 2, 0},
			{l + 1, 3, 0},
			{l + 1, 4, 0},
		}
	}
	if i == 2 && j == 3 {
		return [][3]int{
			{l, 1, 3},
			{l, 2, 4},
			{l, 3, 3},
			{l + 1, 0, 4},
			{l + 1, 1, 4},
			{l + 1, 2, 4},
			{l + 1, 3, 4},
			{l + 1, 4, 4},
		}
	}
	if i == 3 && j == 2 {
		return [][3]int{
			{l, 3, 1},
			{l, 4, 2},
			{l, 3, 3},
			{l + 1, 4, 0},
			{l + 1, 4, 1},
			{l + 1, 4, 2},
			{l + 1, 4, 3},
			{l + 1, 4, 4},
		}
	}
	if i == 0 {
		ret = append(ret, [3]int{l - 1, 1, 2})
		ret = append(ret, [3]int{l, i + 1, j})
	} else if i == 4 {
		ret = append(ret, [3]int{l, i - 1, j})
		ret = append(ret, [3]int{l - 1, 3, 2})
	} else {
		ret = append(ret, [3]int{l, i - 1, j})
		ret = append(ret, [3]int{l, i + 1, j})
	}
	if j == 0 {
		ret = append(ret, [3]int{l - 1, 2, 1})
		ret = append(ret, [3]int{l, i, j + 1})
	} else if j == 4 {
		ret = append(ret, [3]int{l, i, j - 1})
		ret = append(ret, [3]int{l - 1, 2, 3})
	} else {
		ret = append(ret, [3]int{l, i, j - 1})
		ret = append(ret, [3]int{l, i, j + 1})
	}
	return ret
}

func next(bugs map[[3]int]bool) map[[3]int]bool {
	empty := make(map[[3]int]bool)
	ret := make(map[[3]int]bool)
	for k := range bugs {
		neighbours := getNeighbours(k[0], k[1], k[2])
		neighbourCount := 0
		for _, neighbour := range neighbours {
			if _, exists := bugs[neighbour]; exists {
				neighbourCount++
			} else {
				empty[neighbour] = true
			}
		}
		if neighbourCount == 1 {
			ret[k] = true
		}
	}
	for k := range empty {
		neighbours := getNeighbours(k[0], k[1], k[2])
		neighbourCount := 0
		for _, neighbour := range neighbours {
			if _, exists := bugs[neighbour]; exists {
				neighbourCount++
			}
		}
		if neighbourCount == 1 || neighbourCount == 2 {
			ret[k] = true
		}
	}
	return ret
}

func simulate2(input [][]byte) int {
	bugs := make(map[[3]int]bool)
	for i := 0; i < len(input); i++ {
		for j := 0; j < len(input[i]); j++ {
			if input[i][j] == '#' {
				bugs[[3]int{0, i, j}] = true
			}
		}
	}
	for i := 0; i < 200; i++ {
		bugs = next(bugs)
	}
	return len(bugs)
}

func main() {
	input := parse(common.Read("./inputs/example.txt"))
	fmt.Printf("Part 1: %v\n", simulate1(input))
	fmt.Printf("Part 2: %v\n", simulate2(input))
}
