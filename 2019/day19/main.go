package main

import (
	"common"
	"fmt"
)

func solvePart1(program common.Computer) int {
	ret := 0
	for x := 0; x < 50; x++ {
		for y := 0; y < 50; y++ {
			copy := common.CopyIntcodeProgram(&program)
			copy.AppendInput(x)
			copy.AppendInput(y)
			copy.Execute()
			if copy.GetLastOutput() == 1 {
				ret++
			}
		}
	}
	return ret
}

func solvePart2(program common.Computer) int {
	first := 0
	for x := 0; ; x++ {
		for y := first; ; y++ {
			copy := common.CopyIntcodeProgram(&program)
			copy.AppendInput(x)
			copy.AppendInput(y)
			copy.Execute()
			if copy.GetLastOutput() != 1 {
				continue
			}
			first = y
			copy = common.CopyIntcodeProgram(&program)
			copy.AppendInput(x)
			copy.AppendInput(y + 99)
			copy.Execute()
			if copy.GetLastOutput() != 1 {
				break
			}
			copy = common.CopyIntcodeProgram(&program)
			copy.AppendInput(x + 99)
			copy.AppendInput(y)
			copy.Execute()
			if copy.GetLastOutput() != 1 {
				continue
			}
			return 10000*x + y
		}
	}
	return 0
}

func main() {
	program := common.ParseIntcodeProgram(common.Read("./inputs/example.txt"))
	fmt.Printf("Part 1: %v\n", solvePart1(program))
	fmt.Printf("Part 2: %v\n", solvePart2(program))
}
