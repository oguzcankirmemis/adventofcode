package main

import (
	"fmt"
)

func isValid1(num int) bool {
	containsDouble := false
	prev := 10
	for num > 0 {
		digit := num % 10
		if digit == prev {
			containsDouble = true
		}
		if digit > prev {
			return false
		}
		prev = digit
		num = num / 10
	}
	return containsDouble
}

func isValid2(num int) bool {
	containsStrictDouble := false
	prev := 10
	prevOccurs := 1
	for num > 0 {
		digit := num % 10
		if digit == prev {
			prevOccurs++
		} else if prevOccurs == 2 {
			containsStrictDouble = true
			prevOccurs = 1
		} else {
			prevOccurs = 1
		}
		if digit > prev {
			return false
		}
		prev = digit
		num = num / 10
	}
	return containsStrictDouble || prevOccurs == 2
}

func solvePart1() int {
	ret := 0
	for i := 109165; i <= 576723; i++ {
		if isValid1(i) {
			ret++
		}
	}
	return ret
}

func solvePart2() int {
	ret := 0
	for i := 109165; i <= 576723; i++ {
		if isValid2(i) {
			ret++
		}
	}
	return ret
}

func main() {
	fmt.Printf("Part 1: %v\n", solvePart1())
	fmt.Printf("Part 2: %v\n", solvePart2())
}
