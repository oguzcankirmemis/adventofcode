package main

import (
	"common"
	"fmt"
	"math"
)

type Layer [6][25]int

func parse(input string) []Layer {
	ret := make([]Layer, 0)
	for i := 0; i < len(input); {
		var layer Layer
		for j := 0; j < 6; j++ {
			for k := 0; k < 25; k++ {
				layer[j][k] = int(input[i] - '0')
				i++
			}
		}
		ret = append(ret, layer)
	}
	return ret
}

func solvePart1(layers []Layer) int {
	minZeroCount := math.MaxInt
	ret := -1
	for _, l := range layers {
		zeroCount := 0
		oneCount := 0
		twoCount := 0
		for _, r := range l {
			for _, d := range r {
				switch d {
				case 0:
					zeroCount++
				case 1:
					oneCount++
				case 2:
					twoCount++
				}
			}
		}
		if zeroCount < minZeroCount {
			minZeroCount = zeroCount
			ret = oneCount * twoCount
		}
	}
	return ret
}

func renderImage(layers []Layer) {
	var ret [6][25]byte
	for i := 0; i < 6; i++ {
		for j := 0; j < 25; j++ {
			for _, l := range layers {
				if l[i][j] != 2 {
					if l[i][j] == 1 {
						ret[i][j] = '.'
					} else {
						ret[i][j] = 'X'
					}
					break
				}
			}
		}
	}
	for i := 0; i < 6; i++ {
		fmt.Printf("%v\n", string(ret[i][:]))
	}
}

func main() {
	input := parse(common.Read("./inputs/example.txt"))
	fmt.Printf("Part 1: %v\n", solvePart1(input))
	fmt.Printf("Part 2: \n")
	renderImage(input)
}
