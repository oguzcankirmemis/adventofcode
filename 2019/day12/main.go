package main

import (
	"common"
	"fmt"
	"strconv"
	"strings"
)

type vec3 struct {
	x, y, z int
}

type vec4 struct {
	x1, x2, x3, x4 int
}

type moon struct {
	pos, vel vec3
}

func parseMoon(content string) moon {
	var ret moon
	content = strings.TrimFunc(content, func(c rune) bool {
		return c == '<' || c == '>'
	})
	parts := strings.Split(content, ", ")
	tmp, _ := strconv.ParseInt(strings.Split(parts[0], "=")[1], 10, 64)
	ret.pos.x = int(tmp)
	tmp, _ = strconv.ParseInt(strings.Split(parts[1], "=")[1], 10, 64)
	ret.pos.y = int(tmp)
	tmp, _ = strconv.ParseInt(strings.Split(parts[2], "=")[1], 10, 64)
	ret.pos.z = int(tmp)
	return ret
}

func parse(content string) []moon {
	ret := make([]moon, 0)
	for l := range strings.Lines(content) {
		ret = append(ret, parseMoon(strings.TrimSpace(l)))
	}
	return ret
}

func abs(a int) int {
	if a < 0 {
		return -a
	}
	return a
}

func gcd(a, b int) int {
	if b == 0 {
		return a
	}
	if a == 0 {
		return b
	}
	return gcd(b%a, a)
}

func lcm(a, b int) int {
	g := gcd(a, b)
	return (a * b) / g
}

func simulate(moons []moon) {
	for i := 0; i < len(moons); i++ {
		for j := i + 1; j < len(moons); j++ {
			if moons[i].pos.x < moons[j].pos.x {
				moons[i].vel.x++
				moons[j].vel.x--
			}
			if moons[j].pos.x < moons[i].pos.x {
				moons[j].vel.x++
				moons[i].vel.x--
			}
			if moons[i].pos.y < moons[j].pos.y {
				moons[i].vel.y++
				moons[j].vel.y--
			}
			if moons[j].pos.y < moons[i].pos.y {
				moons[j].vel.y++
				moons[i].vel.y--
			}
			if moons[i].pos.z < moons[j].pos.z {
				moons[i].vel.z++
				moons[j].vel.z--
			}
			if moons[j].pos.z < moons[i].pos.z {
				moons[j].vel.z++
				moons[i].vel.z--
			}
		}
	}
	for i := 0; i < len(moons); i++ {
		moons[i].pos.x += moons[i].vel.x
		moons[i].pos.y += moons[i].vel.y
		moons[i].pos.z += moons[i].vel.z
	}
}

func periodX(moons []moon) int {
	v := [4]int{
		moons[0].vel.x,
		moons[1].vel.x,
		moons[2].vel.x,
		moons[3].vel.x,
	}
	p := [4]int{
		moons[0].pos.x,
		moons[1].pos.x,
		moons[2].pos.x,
		moons[3].pos.x,
	}
	k := [2][4]int{
		v,
		p,
	}
	return period(k)
}

func periodY(moons []moon) int {
	v := [4]int{
		moons[0].vel.y,
		moons[1].vel.y,
		moons[2].vel.y,
		moons[3].vel.y,
	}
	p := [4]int{
		moons[0].pos.y,
		moons[1].pos.y,
		moons[2].pos.y,
		moons[3].pos.y,
	}
	k := [2][4]int{
		v,
		p,
	}
	return period(k)
}

func periodZ(moons []moon) int {
	v := [4]int{
		moons[0].vel.z,
		moons[1].vel.z,
		moons[2].vel.z,
		moons[3].vel.z,
	}
	p := [4]int{
		moons[0].pos.z,
		moons[1].pos.z,
		moons[2].pos.z,
		moons[3].pos.z,
	}
	k := [2][4]int{
		v,
		p,
	}
	return period(k)
}

func period(k [2][4]int) int {
	set := make(map[[2][4]int]bool)
	t := 0
	for _, exists := set[k]; !exists; _, exists = set[k] {
		set[k] = true
		for i := 0; i < 4; i++ {
			for j := i + 1; j < 4; j++ {
				if k[1][i] < k[1][j] {
					k[0][i]++
					k[0][j]--
				}
				if k[1][j] < k[1][i] {
					k[0][j]++
					k[0][i]--
				}
			}
		}
		for i := 0; i < 4; i++ {
			k[1][i] += k[0][i]
		}
		t++
	}
	return t
}

func solvePart1(moons []moon) int {
	tmp := make([]moon, len(moons))
	copy(tmp, moons)
	moons = tmp
	for i := 0; i < 1000; i++ {
		simulate(moons)
	}
	energy := 0
	for _, m := range moons {
		energy += (abs(m.pos.x) + abs(m.pos.y) + abs(m.pos.z)) * (abs(m.vel.x) + abs(m.vel.y) + abs(m.vel.z))
	}
	return energy
}

func solvePart2(moons []moon) int {
	px := periodX(moons)
	py := periodY(moons)
	pz := periodZ(moons)
	return lcm(px, lcm(py, pz))
}

func main() {
	input := parse(common.Read("./inputs/example.txt"))
	fmt.Printf("Part 1: %v\n", solvePart1(input))
	fmt.Printf("Part 2: %v\n", solvePart2(input))
}
