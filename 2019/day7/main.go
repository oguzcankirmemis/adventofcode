package main

import (
	"common"
	"fmt"
)

type Computer = common.Computer

func findHighestOutputSignalWithoutLoop(c Computer) int {
	ret := -1
	num_of_combinations := 5 * 5 * 5 * 5 * 5 * 5
	for i := range num_of_combinations {
		c1 := common.CopyIntcodeProgram(&c)
		c2 := common.CopyIntcodeProgram(&c)
		c3 := common.CopyIntcodeProgram(&c)
		c4 := common.CopyIntcodeProgram(&c)
		c5 := common.CopyIntcodeProgram(&c)
		t := i
		p1 := t % 5
		t /= 5
		c1.AppendInput(p1)
		c1.AppendInput(0)
		c1.Execute()
		p2 := t % 5
		if p2 == p1 {
			continue
		}
		t /= 5
		c2.AppendInput(p2)
		c2.AppendInput(c1.GetLastOutput())
		c2.Execute()
		p3 := t % 5
		if p3 == p1 || p3 == p2 {
			continue
		}
		t /= 5
		c3.AppendInput(p3)
		c3.AppendInput(c2.GetLastOutput())
		c3.Execute()
		p4 := t % 5
		if p4 == p1 || p4 == p2 || p4 == p3 {
			continue
		}
		t /= 5
		c4.AppendInput(p4)
		c4.AppendInput(c3.GetLastOutput())
		c4.Execute()
		p5 := t % 5
		if p5 == p1 || p5 == p2 || p5 == p3 || p5 == p4 {
			continue
		}
		c5.AppendInput(p5)
		c5.AppendInput(c4.GetLastOutput())
		c5.Execute()
		o := c5.GetLastOutput()
		if o > ret {
			ret = o
		}
	}
	return ret
}

func findHighestOutputSignalWithLoop(c Computer) int {
	ret := -1
	num_of_combinations := 5 * 5 * 5 * 5 * 5 * 5
loop:
	for i := range num_of_combinations {
		var ps [5]int
		t := i
		for i := range 5 {
			ps[i] = (t % 5) + 5
			for j := range i {
				if ps[j] == ps[i] {
					continue loop
				}
			}
			t /= 5
		}
		var cs [5]Computer
		for i := range 5 {
			cs[i] = common.CopyIntcodeProgram(&c)
			cs[i].AppendInput(ps[i])
		}
		cs[0].AppendInput(0)
		for !cs[4].IsHalted() {
			for i := range 5 {
				cs[i].Execute()
				o := cs[i].GetLastOutput()
				ni := (i + 1) % 5
				cs[ni].AppendInput(o)
			}
		}
		o := cs[4].GetLastOutput()
		if o > ret {
			ret = o
		}
	}
	return ret
}

func main() {
	input := common.ParseIntcodeProgram(common.Read("./inputs/example.txt"))
	fmt.Printf("Part 1: %v\n", findHighestOutputSignalWithoutLoop(input))
	fmt.Printf("Part 2: %v\n", findHighestOutputSignalWithLoop(input))
}
