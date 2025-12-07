package main

import (
	"common"
	"fmt"
	"math"
	"strconv"
	"strings"
)

type ShuffleType int

const (
	DealWithIncrement ShuffleType = iota
	DealIntoNewStack
	Cut
)

type Instruction struct {
	shuffleType ShuffleType
	offset      int
}

func parseInstruction(input string) Instruction {
	parts := strings.Split(input, " ")
	if parts[len(parts)-1] == "stack" {
		return Instruction{
			shuffleType: DealIntoNewStack,
		}
	}
	offset, _ := strconv.ParseInt(parts[len(parts)-1], 10, 64)
	if parts[0] == "cut" {
		return Instruction{
			shuffleType: Cut,
			offset:      int(offset),
		}
	}
	return Instruction{
		shuffleType: DealWithIncrement,
		offset:      int(offset),
	}
}

func parse(input string) []Instruction {
	ret := make([]Instruction, 0)
	for line := range strings.Lines(input) {
		ret = append(ret, parseInstruction(strings.TrimSpace(line)))
	}
	return ret
}

func prepareCards(length int) []int {
	ret := make([]int, length)
	for i := 0; i < length; i++ {
		ret[i] = i
	}
	return ret
}

func dealIntoNewStack(cards []int) {
	for i := 0; i < len(cards)/2; i++ {
		tmp := cards[i]
		cards[i] = cards[len(cards)-i-1]
		cards[len(cards)-i-1] = tmp
	}
}

func cut(cards []int, offset int) {
	if offset < 0 {
		offset = len(cards) + offset
	}
	cutCards := make([]int, offset)
	copy(cutCards, cards)
	copy(cards, cards[offset:])
	copy(cards[len(cards)-offset:], cutCards)
}

func dealWithIncrement(cards []int, offset int) {
	copyCards := make([]int, len(cards))
	copyCards[0] = cards[0]
	j := 0
	for i := 0; i < len(cards); i++ {
		copyCards[j] = cards[i]
		j = (j + offset) % len(cards)
	}
	copy(cards, copyCards)
}

func extendedGcd(a, b int) int {
	s := 0
	r := b
	oldS := 1
	oldR := a
	for r != 0 {
		quotient := oldR / r
		tmp := r
		r = oldR - quotient*r
		oldR = tmp
		tmp = s
		s = oldS - quotient*s
		oldS = tmp
	}
	return oldS
}

func overflowSafeModuloMultiply(a, b, mod int) int {
	if a > math.MaxInt/b {
		rem := 0
		if b%2 == 1 {
			rem = a
		}
		sub := overflowSafeModuloMultiply(a, b/2, mod)
		return (((sub + sub) % mod) + rem) % mod
	}
	return (a * b) % mod
}

func reverseCut(slope, shift, mod, offset int) (newSlope, newShift int) {
	shift += offset
	if shift < 0 {
		shift = -((-shift) % mod)
	} else {
		shift = shift % mod
	}
	return slope, shift
}

func reverseDealIntoNewStack(slope, shift, mod int) (newSlope, newShift int) {
	slope = -slope
	shift = -shift - 1
	if shift < 0 {
		shift = -((-shift) % mod)
	} else {
		shift = shift % mod
	}
	return slope, shift
}

func reverseDealWithIncrement(slope, shift, mod, offset int) (newSlope, newShift int) {
	inverse := extendedGcd(offset, mod)
	absInverse := inverse
	if inverse < 0 {
		absInverse = -inverse
	}
	absSlope := slope
	if slope < 0 {
		absSlope = -slope
	}
	absShift := shift
	if shift < 0 {
		absShift = -shift
	}
	signSlopeInverse := 1
	if (inverse < 0 && slope >= 0) || (inverse >= 0 && slope < 0) {
		signSlopeInverse = -1
	}
	signSlopeShift := 1
	if (inverse < 0 && shift >= 0) || (inverse >= 0 && shift < 0) {
		signSlopeShift = -1
	}
	slope = signSlopeInverse * overflowSafeModuloMultiply(absSlope, absInverse, mod)
	shift = signSlopeShift * overflowSafeModuloMultiply(absShift, absInverse, mod)
	return slope, shift
}

func reverse(instructions []Instruction, cards int) (slope, shift int) {
	slope = 1
	shift = 0
	for i := len(instructions) - 1; i >= 0; i-- {
		switch instructions[i].shuffleType {
		case Cut:
			slope, shift = reverseCut(slope, shift, cards, instructions[i].offset)
		case DealIntoNewStack:
			slope, shift = reverseDealIntoNewStack(slope, shift, cards)
		case DealWithIncrement:
			slope, shift = reverseDealWithIncrement(slope, shift, cards, instructions[i].offset)
		}
	}
	return slope, shift
}

func chain(slope1, shift1, slope2, shift2, mod int) (newSlope, newShift int) {
	absSlope1 := slope1
	if slope1 < 0 {
		absSlope1 = -slope1
	}
	absShift1 := shift1
	if shift1 < 0 {
		absShift1 = -shift1
	}
	absSlope2 := slope2
	if slope2 < 0 {
		absSlope2 = -slope2
	}
	signSlope1Slope2 := 1
	if (slope1 < 0 && slope2 >= 0) || (slope1 >= 0 && slope2 < 0) {
		signSlope1Slope2 = -1
	}
	signShift1Slope2 := 1
	if (shift1 < 0 && slope2 >= 0) || (shift1 >= 0 && slope2 < 0) {
		signShift1Slope2 = -1
	}
	newSlope = signSlope1Slope2 * overflowSafeModuloMultiply(absSlope1, absSlope2, mod)
	newShift = signShift1Slope2 * overflowSafeModuloMultiply(absShift1, absSlope2, mod)
	newShift += shift2
	if newShift < 0 {
		newShift = -((-newShift) % mod)
	} else {
		newShift = newShift % mod
	}
	return newSlope, newShift
}

func exponentiate(slope, shift, mod, times int) (newSlope, newShift int) {
	currSlope := 1
	currShift := 0
	for times > 0 {
		if times%2 == 0 {
			slope, shift = chain(slope, shift, slope, shift, mod)
			times /= 2
		} else {
			currSlope, currShift = chain(currSlope, currShift, slope, shift, mod)
			times -= 1
		}
	}
	return currSlope, currShift
}

func solvePart1(instructions []Instruction) int {
	cards := prepareCards(10007)
	for _, instruction := range instructions {
		switch instruction.shuffleType {
		case Cut:
			cut(cards, instruction.offset)
		case DealIntoNewStack:
			dealIntoNewStack(cards)
		case DealWithIncrement:
			dealWithIncrement(cards, instruction.offset)
		}
	}
	for i, card := range cards {
		if card == 2019 {
			return i
		}
	}
	return -1
}

func solvePart2(instructions []Instruction) int {
	cards := 119315717514047
	times := 101741582076661
	target := 2020
	slope, shift := reverse(instructions, cards)
	slope, shift = exponentiate(slope, shift, cards, times)
	if slope < 0 {
		target = overflowSafeModuloMultiply(-slope, target, cards)
		target = -target
	} else {
		target = overflowSafeModuloMultiply(slope, target, cards)
	}
	target = target + shift
	if target < 0 {
		target = cards - ((-target) % cards)
	} else {
		target = target % cards
	}
	return target
}

func main() {
	input := parse(common.Read("./inputs/example.txt"))
	fmt.Printf("Part 1: %v\n", solvePart1(input))
	fmt.Printf("Part 2: %v\n", solvePart2(input))
}
