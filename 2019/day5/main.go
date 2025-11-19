package main

import (
	"common"
	"fmt"
)

type Computer = common.Computer

func solvePart1(input Computer) int {
	computer := common.CopyIntcodeProgram(&input)
	computer.AppendInput(1)
	computer.Execute()
	return computer.GetLastOutput()
}

func solvePart2(input Computer) int {
	computer := common.CopyIntcodeProgram(&input)
	computer.AppendInput(5)
	computer.Execute()
	return computer.GetLastOutput()
}

func main() {
	input := common.ParseIntcodeProgram(common.Read("./inputs/example.txt"))
	fmt.Printf("Part 1: %v\n", solvePart1(input))
	fmt.Printf("Part 2: %v\n", solvePart2(input))
}
