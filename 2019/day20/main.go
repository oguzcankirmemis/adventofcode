package main

import (
	"common"
	"fmt"
	"math"
	"strings"
)

type maze struct {
	view               [][]byte
	teleports          map[[2]int][2]int
	startRow, startCol int
	endRow, endCol     int
}

func parse(input string) maze {
	ret := maze{
		view:      make([][]byte, 0),
		teleports: make(map[[2]int][2]int),
		startRow:  -1,
		startCol:  -1,
		endRow:    -1,
		endCol:    -1,
	}
	teleports := make(map[string][2][2]int)
	for line := range strings.Lines(input) {
		ret.view = append(ret.view, []byte(strings.Trim(line, "\r\n")))
	}
	for i := 0; i < len(ret.view); i++ {
		for j := 0; j < len(ret.view[i]); j++ {
			if ret.view[i][j] < 'A' || ret.view[i][j] > 'Z' {
				continue
			}
			var label string
			targetRow := i
			targetCol := j
			if j+1 < len(ret.view[i]) && ret.view[i][j+1] >= 'A' && ret.view[i][j+1] <= 'Z' {
				if j-1 > 0 && ret.view[i][j-1] == '.' {
					targetCol = j - 1
				} else {
					targetCol = j + 2
				}
				label = string([]byte{ret.view[i][j], ret.view[i][j+1]})
			} else if i+1 < len(ret.view) && ret.view[i+1][j] >= 'A' && ret.view[i+1][j] <= 'Z' {
				if i-1 > 0 && ret.view[i-1][j] == '.' {
					targetRow = i - 1
				} else {
					targetRow = i + 2
				}
				label = string([]byte{ret.view[i][j], ret.view[i+1][j]})
			} else {
				continue
			}
			if label == "AA" {
				ret.startRow = targetRow
				ret.startCol = targetCol
			} else if label == "ZZ" {
				ret.endRow = targetRow
				ret.endCol = targetCol
			} else if el, exists := teleports[label]; exists {
				teleports[label] = [2][2]int{el[0], {targetRow, targetCol}}
			} else {
				teleports[label] = [2][2]int{{targetRow, targetCol}, {-1, -1}}
			}
		}
	}
	dirs := [4][2]int{{0, 1}, {0, -1}, {1, 0}, {-1, 0}}
	for _, val := range teleports {
		for _, dir := range dirs {
			newRow := val[0][0] + dir[0]
			newCol := val[0][1] + dir[1]
			if 0 <= newRow && newRow < len(ret.view) && 0 <= newCol && newCol < len(ret.view[newRow]) &&
				ret.view[newRow][newCol] >= 'A' && ret.view[newRow][newCol] <= 'Z' {
				ret.teleports[[2]int{newRow, newCol}] = val[1]
			}
			newRow = val[1][0] + dir[0]
			newCol = val[1][1] + dir[1]
			if 0 <= newRow && newRow < len(ret.view) && 0 <= newCol && newCol < len(ret.view[newRow]) &&
				ret.view[newRow][newCol] >= 'A' && ret.view[newRow][newCol] <= 'Z' {
				ret.teleports[[2]int{newRow, newCol}] = val[0]
			}
		}
	}
	return ret
}

func dfs1(input maze) int {
	ret := math.MaxInt
	stack := make([][3]int, 0)
	explored := make(map[[2]int]int)
	stack = append(stack, [3]int{input.startRow, input.startCol, 0})
	dirs := [4][2]int{{0, 1}, {0, -1}, {1, 0}, {-1, 0}}
	for len(stack) > 0 {
		el := stack[len(stack)-1]
		r := el[0]
		c := el[1]
		s := el[2]
		k := [2]int{r, c}
		stack = stack[:len(stack)-1]
		if s >= ret {
			continue
		}
		if r == input.endRow && c == input.endCol {
			ret = s
			continue
		}
		if cost, exists := explored[k]; exists && cost <= s {
			continue
		}
		explored[k] = s
		for _, dir := range dirs {
			nr := r + dir[0]
			nc := c + dir[1]
			nk := [2]int{nr, nc}
			ns := s + 1
			if nel, exists := input.teleports[nk]; exists {
				nr = nel[0]
				nc = nel[1]
				nk = [2]int{nr, nc}
			}
			if input.view[nr][nc] != '.' {
				continue
			}
			if cost, exists := explored[nk]; !exists || ns < cost {
				stack = append(stack, [3]int{nr, nc, ns})
			}
		}
	}
	return ret
}

func isOuter(input maze, row, col int) bool {
	return row == 2 || col == 2 || row == len(input.view)-3 || col == len(input.view[row])-3
}

func dfs2(input maze, maxLevel int) int {
	ret := math.MaxInt
	stack := make([][4]int, 0)
	explored := make(map[[3]int]int)
	stack = append(stack, [4]int{input.startRow, input.startCol, 0, 0})
	dirs := [4][2]int{{0, 1}, {0, -1}, {1, 0}, {-1, 0}}
	for len(stack) > 0 {
		el := stack[len(stack)-1]
		r := el[0]
		c := el[1]
		l := el[2]
		s := el[3]
		k := [3]int{r, c, l}
		stack = stack[:len(stack)-1]
		if l > maxLevel {
			continue
		}
		if s >= ret {
			continue
		}
		if r == input.endRow && c == input.endCol && l == 0 {
			ret = s
			continue
		}
		if cost, exists := explored[k]; exists && cost <= s {
			continue
		}
		explored[k] = s
		for _, dir := range dirs {
			nr := r + dir[0]
			nc := c + dir[1]
			nl := l
			nk := [3]int{nr, nc, nl}
			ns := s + 1
			if nel, exists := input.teleports[[2]int{nr, nc}]; exists {
				if isOuter(input, r, c) && l == 0 {
					continue
				}
				nr = nel[0]
				nc = nel[1]
				if isOuter(input, r, c) {
					nl = l - 1
				} else {
					nl = l + 1
				}
				nk = [3]int{nr, nc, nl}
			}
			if input.view[nr][nc] != '.' {
				continue
			}
			if cost, exists := explored[nk]; !exists || ns < cost {
				stack = append(stack, [4]int{nr, nc, nl, ns})
			}
		}
	}
	return ret
}

func solvePart1(input maze) int {
	return dfs1(input)
}

func solvePart2(input maze) int {
	for i := 1; ; i++ {
		ret := dfs2(input, i)
		if ret != math.MaxInt {
			return ret
		}
	}
}

func main() {
	input := parse(common.Read("./inputs/example.txt"))
	fmt.Printf("Part 1: %v\n", solvePart1(input))
	fmt.Printf("Part 2: %v\n", solvePart2(input))
}
