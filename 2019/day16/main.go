package main

import (
	"common"
	"fmt"
)

func inc(index, patternLength, outer, inner int) (newOuter int, newInner int) {
	newInner = inner + 1
	if newInner >= index+1 {
		newInner = 0
		newOuter = (outer + 1) % patternLength
	} else {
		newOuter = outer
	}
	return newOuter, newInner
}

func fft(input []int) {
	base := [4]int{0, 1, 0, -1}
	for i := 0; i < len(input); i++ {
		res := 0
		outer := 0
		inner := 0
		outer, inner = inc(i, len(base), outer, inner)
		for j := 0; j < len(input); j++ {
			res += input[j] * base[outer]
			outer, inner = inc(i, len(base), outer, inner)
		}
		if res < 0 {
			res = -res
		}
		input[i] = res % 10
	}
}

func solvePart1(input []int) int {
	for i := 0; i < 1; i++ {
		fft(input)
	}
	res := 0
	for i := 0; i < 8; i++ {
		res = 10*res + input[i]
	}
	return res
}

func solvePart2(input []int) int {
	for i := 0; i < 100; i++ {
		curr := 0
		for j := len(input) - 1; j >= len(input)/2; j-- {
			curr += input[j]
			input[j] = curr % 10
		}
	}
	offset := 0
	for i := 0; i < 7; i++ {
		offset = 10*offset + input[i]
	}
	res := 0
	for i := 0; i < 8; i++ {
		res = 10*res + input[offset+i]
	}
	return res
}

func parse(input string) []int {
	ret := make([]int, 0)
	for _, c := range input {
		ret = append(ret, int(c)-'0')
	}
	return ret
}

func main() {
	input := parse(common.Read("./inputs/example.txt"))
	input2 := make([]int, 10000*len(input))
	for i := 0; i < 10000; i++ {
		copy(input2[len(input)*i:len(input)*i+len(input)], input)
	}
	fmt.Printf("Part 1: %v\n", solvePart1(input))
	fmt.Printf("Part 2: %v\n", solvePart2(input2))
}
