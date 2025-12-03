package common

import (
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
	RelativeBaseOffset
	Halt = 99
)

type Computer struct {
	memory  []int
	inputs  []int
	outputs []int
	i       int
	ip      int
	rb      int
	halted  bool
}

func ParseIntcodeProgram(content string) Computer {
	nums := make([]int, 0)
	for s := range strings.SplitSeq(content, ",") {
		num, err := strconv.ParseInt(s, 10, 64)
		if err != nil {
			panic(err)
		}
		nums = append(nums, int(num))
	}
	return Computer{
		memory:  nums,
		inputs:  make([]int, 0),
		outputs: make([]int, 0),
		i:       0,
		ip:      0,
		rb:      0,
		halted:  false,
	}
}

func CopyIntcodeProgram(computer *Computer) Computer {
	memoryCopy := make([]int, len(computer.memory))
	copy(memoryCopy, computer.memory)
	inputCopy := make([]int, len(computer.inputs))
	copy(inputCopy, computer.inputs)
	outputCopy := make([]int, len(computer.outputs))
	copy(outputCopy, computer.outputs)
	return Computer{
		memory:  memoryCopy,
		inputs:  inputCopy,
		outputs: outputCopy,
		i:       computer.i,
		ip:      computer.ip,
		rb:      computer.rb,
		halted:  computer.halted,
	}
}

func (computer *Computer) AppendInput(input int) {
	computer.inputs = append(computer.inputs, input)
}

func (computer *Computer) AppendInputs(input []int) {
	computer.inputs = append(computer.inputs, input...)
}

func (computer *Computer) AppendStringInput(input string) {
	byteSlice := []byte(input)
	intSlice := make([]int, len(byteSlice))
	for i, b := range byteSlice {
		intSlice[i] = int(b)
	}
	computer.inputs = append(computer.inputs, intSlice...)
}

func (computer *Computer) GetLastOutput() int {
	return computer.outputs[len(computer.outputs)-1]
}

func (computer *Computer) PopLastOutput() int {
	ret := computer.outputs[len(computer.outputs)-1]
	computer.outputs = computer.outputs[:len(computer.outputs)-1]
	return ret
}

func (computer *Computer) GetOutput() []int {
	return computer.outputs
}

func (computer *Computer) ClearOutput() {
	computer.outputs = computer.outputs[:0]
}

func (computer *Computer) IsHalted() bool {
	return computer.halted
}

func (computer *Computer) SetMemory(index, val int) {
	for len(computer.memory) <= index {
		computer.memory = append(computer.memory, 0)
	}
	computer.memory[index] = val
}

func (computer *Computer) GetMemory(index int) int {
	if len(computer.memory) <= index {
		return 0
	}
	return computer.memory[index]
}

func (computer *Computer) read(param, mode int) int {
	switch mode % 10 {
	case 0:
		for len(computer.memory) <= param {
			computer.memory = append(computer.memory, 0)
		}
		return computer.memory[param]
	case 1:
		return param
	case 2:
		for len(computer.memory) <= param+computer.rb {
			computer.memory = append(computer.memory, 0)
		}
		return computer.memory[param+computer.rb]
	default:
		panic(mode)
	}
}

func (computer *Computer) write(val, param, mode int) {
	switch mode % 10 {
	case 0:
		for len(computer.memory) <= param {
			computer.memory = append(computer.memory, 0)
		}
		computer.memory[param] = val
	case 1:
		panic(mode)
	case 2:
		for len(computer.memory) <= param+computer.rb {
			computer.memory = append(computer.memory, 0)
		}
		computer.memory[param+computer.rb] = val
	default:
		panic(mode)
	}
}

func (computer *Computer) add(mode int) {
	param1 := computer.memory[computer.ip+1]
	param2 := computer.memory[computer.ip+2]
	param3 := computer.memory[computer.ip+3]
	val1 := computer.read(param1, mode)
	val2 := computer.read(param2, mode/10)
	computer.write(val1+val2, param3, mode/100)
	computer.ip += 4
}

func (computer *Computer) multiply(mode int) {
	param1 := computer.memory[computer.ip+1]
	param2 := computer.memory[computer.ip+2]
	param3 := computer.memory[computer.ip+3]
	val1 := computer.read(param1, mode)
	val2 := computer.read(param2, mode/10)
	computer.write(val1*val2, param3, mode/100)
	computer.ip += 4
}

func (computer *Computer) input(mode int) bool {
	if computer.i >= len(computer.inputs) {
		return false
	}
	param1 := computer.memory[computer.ip+1]
	computer.write(computer.inputs[computer.i], param1, mode)
	computer.i++
	computer.ip += 2
	return true
}

func (computer *Computer) output(mode int) {
	param1 := computer.memory[computer.ip+1]
	val := computer.read(param1, mode)
	computer.outputs = append(computer.outputs, val)
	computer.ip += 2
}

func (computer *Computer) jumpIfTrue(mode int) {
	param1 := computer.memory[computer.ip+1]
	if computer.read(param1, mode) != 0 {
		param2 := computer.memory[computer.ip+2]
		computer.ip = computer.read(param2, mode/10)
	} else {
		computer.ip += 3
	}
}

func (computer *Computer) jumpIfFalse(mode int) {
	param1 := computer.memory[computer.ip+1]
	if computer.read(param1, mode) == 0 {
		param2 := computer.memory[computer.ip+2]
		computer.ip = computer.read(param2, mode/10)
	} else {
		computer.ip += 3
	}
}

func (computer *Computer) lessThan(mode int) {
	param1 := computer.memory[computer.ip+1]
	param2 := computer.memory[computer.ip+2]
	param3 := computer.memory[computer.ip+3]
	val1 := computer.read(param1, mode)
	val2 := computer.read(param2, mode/10)
	if val1 < val2 {
		computer.write(1, param3, mode/100)
	} else {
		computer.write(0, param3, mode/100)
	}
	computer.ip += 4
}

func (computer *Computer) equals(mode int) {
	param1 := computer.memory[computer.ip+1]
	param2 := computer.memory[computer.ip+2]
	param3 := computer.memory[computer.ip+3]
	val1 := computer.read(param1, mode)
	val2 := computer.read(param2, mode/10)
	if val1 == val2 {
		computer.write(1, param3, mode/100)
	} else {
		computer.write(0, param3, mode/100)
	}
	computer.ip += 4
}

func (computer *Computer) relativeBaseOffset(mode int) {
	param1 := computer.memory[computer.ip+1]
	computer.rb += computer.read(param1, mode)
	computer.ip += 2
}

func (computer *Computer) halt() {
	computer.halted = true
}

func (computer *Computer) Execute() {
	if computer.halted {
		return
	}
loop:
	for {
		for len(computer.memory) <= computer.ip+3 {
			computer.memory = append(computer.memory, 0)
		}
		opcode := computer.memory[computer.ip] % 100
		mode := computer.memory[computer.ip] / 100
		switch InstructionType(opcode) {
		case Add:
			computer.add(mode)
		case Multiply:
			computer.multiply(mode)
		case Input:
			if !computer.input(mode) {
				return
			}
		case Output:
			computer.output(mode)
		case JumpIfTrue:
			computer.jumpIfTrue(mode)
		case JumpIfFalse:
			computer.jumpIfFalse(mode)
		case LessThan:
			computer.lessThan(mode)
		case Equals:
			computer.equals(mode)
		case RelativeBaseOffset:
			computer.relativeBaseOffset(mode)
		case Halt:
			computer.halt()
			break loop
		}
	}
}
