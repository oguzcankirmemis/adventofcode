package main

import (
	"common"
	"fmt"
)

func simulate(program *common.Computer) (twiceNatY, firstNatY int) {
	var computers [50]common.Computer
	var queues [50][][2]int
	natPacketsMap := make(map[int]bool)
	firstNatY = -1
	natX := -1
	natY := -1
	for i := 0; i < len(computers); i++ {
		queues[i] = make([][2]int, 0)
		computers[i] = common.CopyIntcodeProgram(program)
		computers[i].AppendInput(i)
	}
	for {
		idleCount := 0
		for i := range computers {
			if computers[i].IsHalted() {
				continue
			}
			noInputReceived := true
			noOutputSent := true
			if len(queues[i]) > 0 {
				packet := queues[i][0]
				queues[i] = queues[i][1:]
				computers[i].AppendInputs(packet[:])
				noInputReceived = false
			} else {
				computers[i].AppendInput(-1)
			}
			computers[i].Execute()
			output := computers[i].GetOutput()
			if len(output) > 0 {
				noOutputSent = false
			}
			for i := 0; i < len(output); i += 3 {
				address := output[i]
				x := output[i+1]
				y := output[i+2]
				if address == 255 {
					if firstNatY == -1 {
						firstNatY = y
					}
					natX = x
					natY = y
				} else {
					queues[address] = append(queues[address], [2]int{x, y})
				}
			}
			computers[i].ClearOutput()
			if noInputReceived && noOutputSent {
				idleCount++
			}
		}
		if idleCount == len(computers) {
			if _, exists := natPacketsMap[natY]; exists {
				return natY, firstNatY
			}
			natPacketsMap[natY] = true
			queues[0] = append(queues[0], [2]int{natX, natY})
		}
	}
}

func main() {
	program := common.ParseIntcodeProgram(common.Read("./inputs/example.txt"))
	twiceNatY, firstNatY := simulate(&program)
	fmt.Printf("Part 1: %v\n", firstNatY)
	fmt.Printf("Part 2: %v\n", twiceNatY)
}
