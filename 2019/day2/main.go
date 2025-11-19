package main

import (
	"common"
	"fmt"
)

type Computer = common.Computer

func solvePart1(input Computer) int {
	computer := common.CopyIntcodeProgram(&input)
	computer.SetMemory(1, 12)
	computer.SetMemory(2, 2)
	computer.Execute()
	return computer.GetMemory(0)
}

func solvePart2(input Computer) int {
	for noun := 0; noun <= 99; noun++ {
		for verb := 0; verb <= 99; verb++ {
			computer := common.CopyIntcodeProgram(&input)
			computer.SetMemory(1, noun)
			computer.SetMemory(2, verb)
			computer.Execute()
			if computer.GetMemory(0) == 19690720 {
				return 100*noun + verb
			}
		}
	}
	return -1
}

func main() {
	input := common.ParseIntcodeProgram(common.Read("./inputs/example.txt"))
	fmt.Printf("Part 1: %v\n", solvePart1(input))
	fmt.Printf("part 2: %v\n", solvePart2(input))
}
