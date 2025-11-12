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

type Computer struct {
	memory []int
	input  []int
	output []int
	i      int
	ip     int
	halted bool
}

func parse(content string) Computer {
	nums := make([]int, 0)
	for s := range strings.SplitSeq(content, ",") {
		num, err := strconv.ParseInt(s, 10, 64)
		if err != nil {
			panic(err)
		}
		nums = append(nums, int(num))
	}
	return Computer{
		memory: nums,
		input:  make([]int, 0),
		output: make([]int, 0),
		i:      0,
		ip:     0,
		halted: false,
	}
}

func copyProgram(computer *Computer) Computer {
	memory_copy := make([]int, len(computer.memory))
	copy(memory_copy, computer.memory)
	return Computer{
		memory: memory_copy,
		input:  make([]int, 0),
		output: make([]int, 0),
		i:      0,
		ip:     0,
		halted: false,
	}
}

func execute(computer *Computer) {
	if computer.halted {
		return
	}
	program := computer.memory
	input := computer.input
	output := computer.output
	i := computer.i
	ip := computer.ip
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
			if i >= len(input) {
				computer.memory = program
				computer.input = input
				computer.output = output
				computer.i = i
				computer.ip = ip
				return
			}
			program[program[ip+1]] = input[i]
			i++
			ip += 2
		case Output:
			param1 := program[ip+1]
			if modes%10 == 0 {
				param1 = program[param1]
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
			computer.halted = true
			break loop
		}
	}
	computer.memory = program
	computer.input = input
	computer.output = output
	computer.i = i
	computer.ip = ip
}

func findHighestOutputSignalWithoutLoop(c Computer) int {
	ret := -1
	num_of_combinations := 5 * 5 * 5 * 5 * 5 * 5
	for i := range num_of_combinations {
		c1 := copyProgram(&c)
		c2 := copyProgram(&c)
		c3 := copyProgram(&c)
		c4 := copyProgram(&c)
		c5 := copyProgram(&c)
		t := i
		p1 := t % 5
		t /= 5
		c1.input = append(c1.input, p1)
		c1.input = append(c1.input, 0)
		execute(&c1)
		p2 := t % 5
		if p2 == p1 {
			continue
		}
		t /= 5
		c2.input = append(c2.input, p2)
		c2.input = append(c2.input, c1.output[len(c1.output)-1])
		execute(&c2)
		p3 := t % 5
		if p3 == p1 || p3 == p2 {
			continue
		}
		t /= 5
		c3.input = append(c3.input, p3)
		c3.input = append(c3.input, c2.output[len(c2.output)-1])
		execute(&c3)
		p4 := t % 5
		if p4 == p1 || p4 == p2 || p4 == p3 {
			continue
		}
		t /= 5
		c4.input = append(c4.input, p4)
		c4.input = append(c4.input, c3.output[len(c3.output)-1])
		execute(&c4)
		p5 := t % 5
		if p5 == p1 || p5 == p2 || p5 == p3 || p5 == p4 {
			continue
		}
		c5.input = append(c5.input, p5)
		c5.input = append(c5.input, c4.output[len(c4.output)-1])
		execute(&c5)
		o := c5.output[len(c5.output)-1]
		if o > ret {
			ret = o
		}
	}
	return ret
}

func findHighestOutputSignalWithLoop(c Computer) int {
	ret := -1
	num_of_combinations := 5 * 5 * 5 * 5 * 5 * 5
loop:
	for i := range num_of_combinations {
		var ps [5]int
		t := i
		for i := range 5 {
			ps[i] = (t % 5) + 5
			for j := range i {
				if ps[j] == ps[i] {
					continue loop
				}
			}
			t /= 5
		}
		var cs [5]Computer
		for i := range 5 {
			cs[i] = copyProgram(&c)
			cs[i].input = append(cs[i].input, ps[i])
		}
		cs[0].input = append(cs[0].input, 0)
		for !cs[4].halted {
			for i := range 5 {
				execute(&cs[i])
				o := cs[i].output[len(cs[i].output)-1]
				ni := (i + 1) % 5
				cs[ni].input = append(cs[ni].input, o)
			}
		}
		o := cs[4].output[len(cs[4].output)-1]
		if o > ret {
			ret = o
		}
	}
	return ret
}

func main() {
	input := parse(common.Read("./inputs/example.txt"))
	fmt.Printf("Part 1: %v\n", findHighestOutputSignalWithoutLoop(input))
	fmt.Printf("Part 2: %v\n", findHighestOutputSignalWithLoop(input))
}
