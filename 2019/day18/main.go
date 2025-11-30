package main

import (
	"common"
	"fmt"
	"math"
	"strings"
)

func parse(input string) [][]byte {
	ret := make([][]byte, 0)
	for line := range strings.Lines(input) {
		ret = append(ret, []byte(strings.TrimSpace(line)))
	}
	return ret
}

func startPoint(input [][]byte) (int, int) {
	for i := 0; i < len(input); i++ {
		for j := 0; j < len(input[i]); j++ {
			if input[i][j] == '@' {
				return i, j
			}
		}
	}
	return -1, -1
}

func target(input [][]byte) int {
	ret := 0
	for i := 0; i < len(input); i++ {
		for j := 0; j < len(input[i]); j++ {
			if input[i][j] >= 'a' && input[i][j] <= 'z' {
				ret |= 1 << (input[i][j] - 'a')
			}
		}
	}
	return ret
}

func bfs(input [][]byte) int {
	dirs := [4][2]int{
		{0, 1},
		{1, 0},
		{-1, 0},
		{0, -1},
	}
	startRow, startCol := startPoint(input)
	targetKeys := target(input)
	queue := make([][4]int, 0)
	explored := make(map[[3]int]int)
	explored[[3]int{startRow, startCol, 0}] = 0
	queue = append(queue, [4]int{startRow, startCol, 0, 0})
	for len(queue) > 0 {
		el := queue[0]
		r := el[0]
		c := el[1]
		k := el[2]
		s := el[3]
		queue = queue[1:]
		if k == targetKeys {
			return s
		}
		for _, dir := range dirs {
			nr := r + dir[0]
			nc := c + dir[1]
			nk := k
			ns := s + 1
			if nr < 0 || nr >= len(input) {
				continue
			}
			if nc < 0 || nc >= len(input[nr]) {
				continue
			}
			if input[nr][nc] == '#' {
				continue
			}
			if input[nr][nc] >= 'A' && input[nr][nc] <= 'Z' && k&(1<<(input[nr][nc]-'A')) == 0 {
				continue
			}
			if input[nr][nc] >= 'a' && input[nr][nc] <= 'z' && k&(1<<(input[nr][nc]-'a')) == 0 {
				nk |= 1 << (input[nr][nc] - 'a')
			}
			if _, exists := explored[[3]int{nr, nc, nk}]; exists {
				continue
			}
			explored[[3]int{nr, nc, nk}] = ns
			queue = append(queue, [4]int{nr, nc, nk, ns})
		}
	}
	return -1
}

func bfsAgent(input [][]byte, row, col, keys int) [][4]int {
	ret := make([][4]int, 0)
	dirs := [4][2]int{
		{0, 1},
		{1, 0},
		{-1, 0},
		{0, -1},
	}
	queue := make([][4]int, 0)
	explored := make(map[[2]int]int)
	explored[[2]int{row, col}] = 0
	queue = append(queue, [4]int{row, col, keys, 0})
	for len(queue) > 0 {
		el := queue[0]
		r := el[0]
		c := el[1]
		k := el[2]
		s := el[3]
		queue = queue[1:]
		if k != keys {
			ret = append(ret, el)
			continue
		}
		for _, dir := range dirs {
			nr := r + dir[0]
			nc := c + dir[1]
			nk := k
			ns := s + 1
			if nr < 0 || nr >= len(input) {
				continue
			}
			if nc < 0 || nc >= len(input[nr]) {
				continue
			}
			if input[nr][nc] == '#' {
				continue
			}
			if input[nr][nc] >= 'A' && input[nr][nc] <= 'Z' && k&(1<<(input[nr][nc]-'A')) == 0 {
				continue
			}
			if input[nr][nc] >= 'a' && input[nr][nc] <= 'z' && k&(1<<(input[nr][nc]-'a')) == 0 {
				nk |= 1 << (input[nr][nc] - 'a')
			}
			if _, exists := explored[[2]int{nr, nc}]; exists {
				continue
			}
			explored[[2]int{nr, nc}] = ns
			queue = append(queue, [4]int{nr, nc, nk, ns})
		}
	}
	return ret
}

func dfs(input [][]byte, agents [4][2]int) int {
	ret := math.MaxInt
	targetKeys := target(input)
	stack := make([][5][2]int, 0)
	stack = append(stack, [5][2]int{
		agents[0],
		agents[1],
		agents[2],
		agents[3],
		{0, 0},
	})
	explored := make(map[[9]int]int)
	for len(stack) > 0 {
		el := stack[len(stack)-1]
		agents = [4][2]int{
			el[0],
			el[1],
			el[2],
			el[3],
		}
		keys := el[4][0]
		steps := el[4][1]
		hash := [9]int{
			el[0][0],
			el[0][1],
			el[1][0],
			el[1][1],
			el[2][0],
			el[2][1],
			el[3][0],
			el[3][1],
			keys,
		}
		stack = stack[:len(stack)-1]
		if keys == targetKeys {
			if steps < ret {
				ret = steps
			}
			continue
		}
		if steps >= ret {
			continue
		}
		if oldSteps, exists := explored[hash]; !exists || steps < oldSteps {
			explored[hash] = steps
			for i := range agents {
				for _, searchResult := range bfsAgent(input, agents[i][0], agents[i][1], keys) {
					newAgents := [4][2]int{
						agents[0],
						agents[1],
						agents[2],
						agents[3],
					}
					newAgents[i][0] = searchResult[0]
					newAgents[i][1] = searchResult[1]
					newKeys := searchResult[2]
					newSteps := steps + searchResult[3]
					stack = append(stack, [5][2]int{
						newAgents[0],
						newAgents[1],
						newAgents[2],
						newAgents[3],
						{newKeys, newSteps},
					})
				}
			}
		}
	}
	return ret
}

func updateMap(input [][]byte) [4][2]int {
	startRow, startCol := startPoint(input)
	input[startRow][startCol] = '#'
	input[startRow][startCol-1] = '#'
	input[startRow][startCol+1] = '#'
	input[startRow-1][startCol] = '#'
	input[startRow+1][startCol] = '#'
	input[startRow+1][startCol+1] = '.'
	input[startRow+1][startCol-1] = '.'
	input[startRow-1][startCol+1] = '.'
	input[startRow-1][startCol-1] = '.'
	return [4][2]int{
		{startRow - 1, startCol - 1},
		{startRow + 1, startCol - 1},
		{startRow - 1, startCol + 1},
		{startRow + 1, startCol + 1},
	}
}

func main() {
	input := parse(common.Read("./inputs/example.txt"))
	fmt.Printf("Part 1: %v\n", bfs(input))
	startPoints := updateMap(input)
	fmt.Printf("Part 2: %v\n", dfs(input, startPoints))
}
