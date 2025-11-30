package main

import (
	"common"
	"fmt"
)

func printMap(view [][]byte) {
	for _, row := range view {
		fmt.Printf("%v\n", string(row))
	}
}

func countNeighbours(view [][]byte, row, col int) int {
	neighbours := 0
	if row > 0 && view[row-1][col] == '#' {
		neighbours++
	}
	if row < len(view)-1 && view[row+1][col] == '#' {
		neighbours++
	}
	if col > 0 && view[row][col-1] == '#' {
		neighbours++
	}
	if col < len(view[row])-1 && view[row][col+1] == '#' {
		neighbours++
	}
	return neighbours
}

func sumAlignmentParameters(view [][]byte) int {
	ret := 0
	for i, row := range view {
		for j, c := range row {
			if c == '#' && countNeighbours(view, i, j) >= 3 {
				ret += i * j
			}
		}
	}
	return ret
}

func produceMap(program common.Computer) [][]byte {
	ret := make([][]byte, 0)
	ret = append(ret, make([]byte, 0))
	for !program.IsHalted() {
		program.Execute()
	}
	for _, c := range program.GetOutput() {
		if c == '\n' {
			ret = append(ret, make([]byte, 0))
		} else {
			ret[len(ret)-1] = append(ret[len(ret)-1], byte(c))
		}
	}
	for len(ret[len(ret)-1]) == 0 {
		ret = ret[:len(ret)-1]
	}
	return ret
}

func convertToInput(routine, movementA, movementB, movementC, lineFeed string) []int {
	ret := make([]int, 0)
	for i := 0; i < len(routine); i++ {
		ret = append(ret, int(routine[i]))
	}
	for i := 0; i < len(movementA); i++ {
		ret = append(ret, int(movementA[i]))
	}
	for i := 0; i < len(movementB); i++ {
		ret = append(ret, int(movementB[i]))
	}
	for i := 0; i < len(movementC); i++ {
		ret = append(ret, int(movementC[i]))
	}
	for i := 0; i < len(lineFeed); i++ {
		ret = append(ret, int(lineFeed[i]))
	}
	return ret
}

func play(program common.Computer) int {
	program.SetMemory(0, 2)
	routine := "A,B,A,A,B,C,B,C,C,B\n"
	movementA := "L,12,R,8,L,6,R,8,L,6\n"
	movementB := "R,8,L,12,L,12,R,8\n"
	movementC := "L,6,R,6,L,12\n"
	feed := "n\n"
	input := convertToInput(routine, movementA, movementB, movementC, feed)
	program.AppendInputs(input)
	program.Execute()
	for _, c := range program.GetOutput() {
		if c > 255 {
			return c
		}
	}
	return 0
}

func main() {
	program := common.ParseIntcodeProgram(common.Read("./inputs/example.txt"))
	view := produceMap(common.CopyIntcodeProgram(&program))
	fmt.Printf("Part 1: %v\n", sumAlignmentParameters(view))
	fmt.Printf("Part 2: %v\n", play(program))
}
