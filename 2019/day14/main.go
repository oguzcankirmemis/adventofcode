package main

import (
	"common"
	"fmt"
	"math"
	"strconv"
	"strings"
)

type reactionPart struct {
	substance string
	amount    int
}

type reaction struct {
	parts  []reactionPart
	amount int
}

const TOTAL_ORES = 1000000000000

func parsePart(input string) reactionPart {
	parts := strings.Split(input, " ")
	amount, _ := strconv.ParseInt(parts[0], 10, 64)
	return reactionPart{
		substance: parts[1],
		amount:    int(amount),
	}
}

func parse(input string) map[string]reaction {
	ret := make(map[string]reaction)
	for l := range strings.Lines(input) {
		parts := strings.Split(strings.TrimSpace(l), " => ")
		target := parsePart(parts[1])
		ingredients := make([]reactionPart, 0)
		for _, p := range strings.Split(parts[0], ", ") {
			ingredient := parsePart(p)
			ingredients = append(ingredients, ingredient)
		}
		ret[target.substance] = reaction{
			parts:  ingredients,
			amount: target.amount,
		}
	}
	return ret
}

func findRequiredOre(reactionMap map[string]reaction, fuel int, ores int) (int, bool) {
	usedOres := 0
	left := make(map[string]int)
	_ = left
	stack := make([]reactionPart, 0)
	stack = append(stack, reactionPart{
		substance: "FUEL",
		amount:    fuel,
	})
	for ores > 0 && len(stack) > 0 {
		p := stack[len(stack)-1]
		stack = stack[:len(stack)-1]
		if p.substance == "ORE" {
			usedOres += p.amount
			ores -= p.amount
			continue
		}
		if a, exists := left[p.substance]; exists {
			if a >= p.amount {
				left[p.substance] -= p.amount
				p.amount = 0
			} else {
				p.amount -= a
				left[p.substance] = 0
			}
		}
		if p.amount == 0 {
			continue
		}
		r := reactionMap[p.substance]
		m := p.amount / r.amount
		if p.amount%r.amount > 0 {
			m++
		}
		left[p.substance] = m*r.amount - p.amount
		for _, i := range r.parts {
			stack = append(stack, reactionPart{
				substance: i.substance,
				amount:    m * i.amount,
			})
		}
	}
	return usedOres, len(stack) == 0 && ores >= 0
}

func findMaxFuel(reactionMap map[string]reaction, ores, minOresForOneFuel int) int {
	l := TOTAL_ORES / minOresForOneFuel
	r := math.MaxInt - l
	ret := 0
	for l <= r {
		m := l + (r-l)/2
		if _, ok := findRequiredOre(reactionMap, m, ores); ok {
			ret = m
			l = m + 1
		} else {
			r = m - 1
		}
	}
	return ret
}

func main() {
	input := common.Read("./inputs/example.txt")
	reactionMap := parse(input)
	oresForOneFuel, _ := findRequiredOre(reactionMap, 1, TOTAL_ORES)
	fmt.Printf("Part 1: %v\n", oresForOneFuel)
	fmt.Printf("Part 2: %v\n", findMaxFuel(reactionMap, TOTAL_ORES, oresForOneFuel))
}
