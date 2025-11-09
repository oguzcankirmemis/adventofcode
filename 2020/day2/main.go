package main

import (
	"common"
	"fmt"
	"strconv"
	"strings"
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

func execute(input []int, noun, verb int) int {
	input[1] = noun
	input[2] = verb
	i := 0
loop:
	for {
		switch input[i] {
		case 1:
			input[input[i+3]] = input[input[i+1]] + input[input[i+2]]
		case 2:
			input[input[i+3]] = input[input[i+1]] * input[input[i+2]]
		case 99:
			break loop
		}
		i += 4
	}
	return input[0]
}

func solvePart1(input []int) int {
	input_copy := make([]int, len(input))
	copy(input_copy, input)
	return execute(input_copy, 12, 2)
}

func solvePart2(input []int) int {
	for noun := 0; noun <= 99; noun++ {
		for verb := 0; verb <= 99; verb++ {
			input_copy := make([]int, len(input))
			copy(input_copy, input)
			result := execute(input_copy, noun, verb)
			if result == 19690720 {
				return 100*noun + verb
			}
		}
	}
	return -1
}

func main() {
	input := parse(common.Read("./inputs/example.txt"))
	fmt.Printf("Part 1: %v\n", solvePart1(input))
	fmt.Printf("part 2: %v\n", solvePart2(input))
}
