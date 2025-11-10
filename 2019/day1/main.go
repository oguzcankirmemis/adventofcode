package main

import (
	"common"
	"fmt"
	"strconv"
	"strings"
)

func parse(content string) []int {
	masses := make([]int, 0)
	for s := range strings.Lines(content) {
		mass, err := strconv.ParseInt(strings.TrimSpace(s), 10, 64)
		if err != nil {
			panic(err)
		}
		masses = append(masses, int(mass))
	}
	return masses
}

func solvePart1(input []int) int {
	res := 0
	for _, m := range input {
		res += (m / 3) - 2
	}
	return res
}

func solvePart2(input []int) int {
	res := 0
	for _, m := range input {
		for m > 0 {
			m = (m / 3) - 2
			if m > 0 {
				res += m
			}
		}
	}
	return res
}

func main() {
	input := parse(common.Read("./inputs/example.txt"))
	fmt.Printf("Part 1: %v\n", solvePart1(input))
	fmt.Printf("Part 2: %v\n", solvePart2(input))
}
