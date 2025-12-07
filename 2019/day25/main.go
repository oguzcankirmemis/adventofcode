package main

import (
	"common"
	"fmt"
)

func solve(computer *common.Computer) {
	input := []string{
		"north\n",
		"west\n",
		"take planetoid\n",
		"west\n",
		"take spool of cat6\n",
		"east\n",
		"east\n",
		"south\n",
		"east\n",
		"north\n",
		"take sand\n",
		"west\n",
		"take coin\n",
		"north\n",
		"take jam\n",
		"south\n",
		"west\n",
		"south\n",
		"take wreath\n",
		"west\n",
		"take fuel cell\n",
		"east\n",
		"north\n",
		"east\n",
		"east\n",
		"south\n",
		"west\n",
		"west\n",
		"north\n",
		"take dark matter\n",
		"south\n",
		"east\n",
		"east\n",
		"north\n",
		"west\n",
		"west\n",
		"north\n",
		"west\n",
		"inv\n",
		"drop dark matter\n",
		"drop planetoid\n",
		"drop wreath\n",
		"drop coin\n",
		"south\n",
	}
	for _, command := range input {
		computer.AppendStringInput(command)
		computer.Execute()
	}
	output := computer.GetOutput()
	outputStr := make([]byte, len(output))
	for i := 0; i < len(output); i++ {
		outputStr[i] = byte(output[i])
	}
	fmt.Printf("%v\n", string(outputStr))
}

func main() {
	program := common.ParseIntcodeProgram(common.Read("./inputs/example.txt"))
	solve(&program)
}
