package main

import (
	"common"
	"fmt"
)

type Computer = common.Computer

func getBOOSTKeycode(computer Computer) int {
	c := common.CopyIntcodeProgram(&computer)
	c.AppendInput(1)
	c.Execute()
	return c.GetLastOutput()
}

func getBOOSTDistressSignal(computer Computer) int {
	c := common.CopyIntcodeProgram(&computer)
	c.AppendInput(2)
	c.Execute()
	return c.GetLastOutput()
}

func main() {
	input := common.ParseIntcodeProgram(common.Read("./inputs/example.txt"))
	fmt.Printf("Part 1: %v\n", getBOOSTKeycode(input))
	fmt.Printf("Part 2; %v\n", getBOOSTDistressSignal(input))
}
