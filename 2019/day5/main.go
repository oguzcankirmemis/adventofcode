package main

import (
	"common"
	"fmt"
	"strconv"
	"strings"
)

type InstructionType int

const (
	Add InstructionType = iota + 1
	Multiply
	Input
	Output
	JumpIfTrue
	JumpIfFalse
	LessThan
	Equals
	Halt = 99
)

func parse(content string) []int {
	nums := make([]int, 0)
	for s := range strings.SplitSeq(content, ",") {
		num, err := strconv.ParseInt(s, 10, 64)
		if err != nil {
			panic(err)
		}
		nums = append(nums, int(num))
	}
	return nums
}

func execute(program, input []int) int {
	program_copy := make([]int, len(program))
	copy(program_copy, program)
	program = program_copy
	output := make([]int, 0)
	i := 0
	ip := 0
loop:
	for {
		opcode := program[ip] % 100
		modes := program[ip] / 100
		switch InstructionType(opcode) {
		case Add:
			param1 := program[ip+1]
			if modes%10 == 0 {
				param1 = program[param1]
			}
			modes = modes / 10
			param2 := program[ip+2]
			if modes%10 == 0 {
				param2 = program[param2]
			}
			program[program[ip+3]] = param1 + param2
			ip += 4
		case Multiply:
			param1 := program[ip+1]
			if modes%10 == 0 {
				param1 = program[param1]
			}
			modes = modes / 10
			param2 := program[ip+2]
			if modes%10 == 0 {
				param2 = program[param2]
			}
			program[program[ip+3]] = param1 * param2
			ip += 4
		case Input:
			program[program[ip+1]] = input[i]
			i++
			ip += 2
		case Output:
			param1 := program[ip+1]
			if modes%10 == 0 {
				param1 = program[param1]
			}
			if len(output) > 0 && output[len(output)-1] != 0 {
				fmt.Printf("Execution unsuccessful: index is {%v}, output is {%v}", ip, output[len(output)-1])
			}
			output = append(output, param1)
			ip += 2
		case JumpIfTrue:
			param1 := program[ip+1]
			if modes%10 == 0 {
				param1 = program[param1]
			}
			modes = modes / 10
			param2 := program[ip+2]
			if modes%10 == 0 {
				param2 = program[param2]
			}
			if param1 != 0 {
				ip = param2
			} else {
				ip += 3
			}
		case JumpIfFalse:
			param1 := program[ip+1]
			if modes%10 == 0 {
				param1 = program[param1]
			}
			modes = modes / 10
			param2 := program[ip+2]
			if modes%10 == 0 {
				param2 = program[param2]
			}
			if param1 == 0 {
				ip = param2
			} else {
				ip += 3
			}
		case LessThan:
			param1 := program[ip+1]
			if modes%10 == 0 {
				param1 = program[param1]
			}
			modes = modes / 10
			param2 := program[ip+2]
			if modes%10 == 0 {
				param2 = program[param2]
			}
			if param1 < param2 {
				program[program[ip+3]] = 1
			} else {
				program[program[ip+3]] = 0
			}
			ip += 4
		case Equals:
			param1 := program[ip+1]
			if modes%10 == 0 {
				param1 = program[param1]
			}
			modes = modes / 10
			param2 := program[ip+2]
			if modes%10 == 0 {
				param2 = program[param2]
			}
			if param1 == param2 {
				program[program[ip+3]] = 1
			} else {
				program[program[ip+3]] = 0
			}
			ip += 4
		case Halt:
			break loop
		}
	}
	return output[len(output)-1]
}

func main() {
	input := parse(common.Read("./inputs/example.txt"))
	fmt.Printf("Part 1: %v\n", execute(input, []int{1}))
	fmt.Printf("Part 2: %v\n", execute(input, []int{5}))
}
