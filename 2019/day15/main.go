package main

import (
	"common"
	"fmt"
	"maps"
	"math"
)

type vec2 struct {
	x, y int
}

type stackElement struct {
	position vec2
	program  common.Computer
	steps    int
	visited  map[vec2]bool
}

type queueElement struct {
	position vec2
	program  common.Computer
	steps    int
}

func dfs(program common.Computer) (int, common.Computer) {
	stack := make([]stackElement, 0)
	stack = append(stack, stackElement{
		position: vec2{},
		program:  common.CopyIntcodeProgram(&program),
		steps:    0,
		visited:  make(map[vec2]bool),
	})
	minSteps := math.MaxInt
	var minProgram common.Computer
	for len(stack) > 0 {
		e := stack[len(stack)-1]
		stack = stack[:len(stack)-1]
		for d := 1; d <= 4; d++ {
			np := e.position
			switch d {
			case 1:
				np.y++
			case 2:
				np.y--
			case 3:
				np.x--
			case 4:
				np.x++
			}
			if _, exists := e.visited[np]; exists {
				continue
			}
			programCopy := common.CopyIntcodeProgram(&e.program)
			programCopy.AppendInput(d)
			programCopy.Execute()
			o := programCopy.PopLastOutput()
			switch o {
			case 1:
				visitedCopy := maps.Clone(e.visited)
				visitedCopy[e.position] = true
				stack = append(stack, stackElement{
					position: np,
					program:  programCopy,
					steps:    e.steps + 1,
					visited:  visitedCopy,
				})
			case 2:
				if e.steps+1 < minSteps {
					minSteps = e.steps + 1
					minProgram = programCopy
				}
			}
		}
	}
	return minSteps, minProgram
}

func bfs(program common.Computer) int {
	queue := make([]queueElement, 0)
	visited := make(map[vec2]bool)
	visited[vec2{}] = true
	queue = append(queue, queueElement{
		position: vec2{},
		program:  common.CopyIntcodeProgram(&program),
		steps:    0,
	})
	maxSteps := 0
	for len(queue) > 0 {
		e := queue[0]
		queue = queue[1:]
		for d := 1; d <= 4; d++ {
			np := e.position
			switch d {
			case 1:
				np.y++
			case 2:
				np.y--
			case 3:
				np.x--
			case 4:
				np.x++
			}
			if _, exists := visited[np]; exists {
				continue
			}
			programCopy := common.CopyIntcodeProgram(&e.program)
			programCopy.AppendInput(d)
			programCopy.Execute()
			o := programCopy.PopLastOutput()
			switch o {
			case 1:
				visited[np] = true
				queue = append(queue, queueElement{
					position: np,
					program:  programCopy,
					steps:    e.steps + 1,
				})
				if e.steps+1 > maxSteps {
					maxSteps = e.steps + 1
				}
			}
		}
	}
	return maxSteps
}

func main() {
	program := common.ParseIntcodeProgram(common.Read("./inputs/example.txt"))
	minSteps, minStepsProgram := dfs(program)
	fmt.Printf("Part 1: %v\n", minSteps)
	fmt.Printf("Part 2: %v\n", bfs(minStepsProgram))
}
