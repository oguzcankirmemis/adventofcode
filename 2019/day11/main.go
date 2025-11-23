package main

import (
	"common"
	"fmt"
	"math"
)

type point struct {
	x, y int
}

func getVisitedPanels(program *common.Computer, startPanelColor int) map[point]int {
	visitedPanels := make(map[point]int)
	computer := common.CopyIntcodeProgram(program)
	dirs := [4]point{
		{
			x: 0,
			y: 1,
		},
		{
			x: 1,
			y: 0,
		},
		{
			x: 0,
			y: -1,
		},
		{
			x: -1,
			y: 0,
		},
	}
	robot := point{
		x: 0,
		y: 0,
	}
	dir := 0
	visitedPanels[robot] = startPanelColor
	for !computer.IsHalted() {
		if color, exists := visitedPanels[robot]; exists {
			computer.AppendInput(color)
		} else {
			computer.AppendInput(0)
		}
		computer.Execute()
		turn := computer.PopLastOutput()
		color := computer.PopLastOutput()
		visitedPanels[robot] = color
		if turn == 0 {
			dir = dir - 1
			if dir < 0 {
				dir = len(dirs) - 1
			}
		} else {
			dir = (dir + 1) % len(dirs)
		}
		robot.x += dirs[dir].x
		robot.y += dirs[dir].y
	}
	return visitedPanels
}

func printRegistrationIdentifier(program *common.Computer) {
	visitedPanels := getVisitedPanels(program, 1)
	minX := math.MaxInt
	maxX := math.MinInt
	minY := math.MaxInt
	maxY := math.MinInt
	for k, _ := range visitedPanels {
		if k.x < minX {
			minX = k.x
		}
		if k.x > maxX {
			maxX = k.x
		}
		if k.y < minY {
			minY = k.y
		}
		if k.y > maxY {
			maxY = k.y
		}
	}
	output := make([][]byte, maxY-minY+1)
	for i := minY; i <= maxY; i++ {
		output[i-minY] = make([]byte, maxX-minX+1)
	}
	for y, _ := range output {
		for x, _ := range output[y] {
			output[y][x] = '.'
		}
	}
	for k, c := range visitedPanels {
		if c == 1 {
			output[k.y-minY][k.x-minX] = '#'
		} else {
			output[k.y-minY][k.x-minX] = '.'
		}
	}
	for i := len(output) - 1; i >= 0; i-- {
		fmt.Printf("%v\n", string(output[i]))
	}
}

func main() {
	program := common.ParseIntcodeProgram(common.Read("./inputs/example.txt"))
	fmt.Printf("Part 1: %v\n", len(getVisitedPanels(&program, 0)))
	fmt.Printf("Part 2:\n")
	printRegistrationIdentifier(&program)
}
