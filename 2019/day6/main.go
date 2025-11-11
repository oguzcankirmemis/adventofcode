package main

import (
	"common"
	"fmt"
	"strings"
)

type Orbit struct {
	name string
	owns []string
}

type OrbitStackElement struct {
	name  string
	depth int
}

type Ancestor struct {
	name            string
	distanceToYou   int
	distanceToSanta int
}

func parse(input string) map[string]Orbit {
	ret := make(map[string]Orbit)
	for l := range strings.Lines(input) {
		parts := strings.Split(strings.TrimSpace(l), ")")
		parent := parts[0]
		child := parts[1]
		if orbit, exists := ret[parent]; exists {
			orbit.owns = append(orbit.owns, child)
			ret[parent] = orbit
		} else {
			owns := make([]string, 1)
			owns[0] = child
			ret[parent] = Orbit{
				name: parent,
				owns: owns,
			}
		}
		if _, exists := ret[child]; !exists {
			ret[child] = Orbit{
				name: child,
				owns: make([]string, 0),
			}
		}
	}
	return ret
}

func countIndirectOrbits(orbitMap map[string]Orbit) int {
	ret := 0
	stack := make([]OrbitStackElement, 1)
	stack[0] = OrbitStackElement{
		name:  "COM",
		depth: 0,
	}
	for len(stack) > 0 {
		curr := stack[len(stack)-1]
		stack = stack[:len(stack)-1]
		ret += curr.depth
		for _, orbit := range orbitMap[curr.name].owns {
			element := OrbitStackElement{
				name:  orbit,
				depth: curr.depth + 1,
			}
			stack = append(stack, element)
		}
	}
	return ret
}

func findCommonAncestorToSanta(orbitMap map[string]Orbit, current string) Ancestor {
	if current == "YOU" {
		return Ancestor{
			name:            "YOU",
			distanceToYou:   0,
			distanceToSanta: -1,
		}
	}
	if current == "SAN" {
		return Ancestor{
			name:            "SAN",
			distanceToYou:   -1,
			distanceToSanta: 0,
		}
	}
	ret := Ancestor{
		name:            current,
		distanceToSanta: -1,
		distanceToYou:   -1,
	}
	orbit := orbitMap[current]
	for _, o := range orbit.owns {
		a := findCommonAncestorToSanta(orbitMap, o)
		if a.distanceToSanta >= 0 && a.distanceToYou >= 0 {
			return a
		}
		if a.distanceToSanta >= 0 {
			ret.distanceToSanta = a.distanceToSanta + 1
		}
		if a.distanceToYou >= 0 {
			ret.distanceToYou = a.distanceToYou + 1
		}
		if ret.distanceToSanta >= 0 && ret.distanceToYou >= 0 {
			return ret
		}
	}
	return ret
}

func computeDistanceToSanta(orbitMap map[string]Orbit) int {
	ancestor := findCommonAncestorToSanta(orbitMap, "COM")
	return ancestor.distanceToSanta + ancestor.distanceToYou - 2
}

func main() {
	input := common.Read("./inputs/example.txt")
	orbitMap := parse(input)
	fmt.Printf("Part 1: %v\n", countIndirectOrbits(orbitMap))
	fmt.Printf("Part 2: %v\n", computeDistanceToSanta(orbitMap))
}
