package main

import (
	"common"
	"fmt"
)

func solvePart1(program *common.Computer) int {
	computer := common.CopyIntcodeProgram(program)
	springscript := `NOT B T
	NOT C J
	AND T J
	AND D J
	NOT C T
	AND D T
	OR T J
	NOT A T
	OR T J
	WALK
	`
	computer.AppendStringInput(springscript)
	computer.Execute()
	return computer.GetLastOutput()
}

func solvePart2(program *common.Computer) int {
	computer := common.CopyIntcodeProgram(program)
	springscript := `NOT C J
	AND D J
	NOT A T
	OR T J
	AND A T
	OR B T
	OR E T
	NOT T T
	OR T J
	NOT E T
	NOT T T
	OR H T
	AND T J
	RUN
	`
	computer.AppendStringInput(springscript)
	computer.Execute()
	return computer.GetLastOutput()
}

func main() {
	program := common.ParseIntcodeProgram(common.Read("./inputs/example.txt"))
	fmt.Printf("Part 1: %v\n", solvePart1(&program))
	fmt.Printf("Part 2: %v\n", solvePart2(&program))
}
