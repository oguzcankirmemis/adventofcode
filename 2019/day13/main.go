package main

import (
	"common"
	"fmt"
	"strings"

	"github.com/mattn/go-tty"
)

const EMPTY, EMPTY_ID = '.', 0
const WALL, WALL_ID = '#', 1
const BLOCK, BLOCK_ID = 'B', 2
const HORIZONTAL_PADDLE, HORIZONTAL_PADDLE_ID = '-', 3
const BALL, BALL_ID = 'O', 4

const INPUT_TIMEOUT = 100000000

const LEFT = -1
const NEUTRAL = 0
const RIGHT = 1

func draw(gameMap [][]byte) {
	output := make([]string, len(gameMap))
	for i, r := range gameMap {
		output[i] = string(r)
	}
	fmt.Printf("\n\n%v\n\n", strings.Join(output, "\n"))
}

func playGame(program common.Computer, manual bool) (score int, blocks int) {
	program.SetMemory(0, 2)
	gameMap := make([][]byte, 0)
	blocks = 0
	score = 0
	program.Execute()
	output := program.GetOutput()
	var paddleX, ballX int
	for i := 0; i < len(output); i += 3 {
		x := output[i]
		y := output[i+1]
		id := output[i+2]
		if x == -1 && y == 0 {
			score = id
			continue
		}
		for len(gameMap) <= y {
			gameMap = append(gameMap, make([]byte, 0))
		}
		for len(gameMap[y]) <= x {
			gameMap[y] = append(gameMap[y], 0)
		}
		switch id {
		case EMPTY_ID:
			gameMap[y][x] = EMPTY
		case WALL_ID:
			gameMap[y][x] = WALL
		case BLOCK_ID:
			gameMap[y][x] = BLOCK
			blocks++
		case HORIZONTAL_PADDLE_ID:
			gameMap[y][x] = HORIZONTAL_PADDLE
			paddleX = x
		case BALL_ID:
			gameMap[y][x] = BALL
			ballX = x
		}
	}
	tty, err := tty.Open()
	if err != nil {
		panic(err)
	}
	defer tty.Close()
	for !program.IsHalted() {
		if manual {
			fmt.Printf("Score: %v", score)
			draw(gameMap)
		}
		r, err := rune(0), error(nil)
		if manual {
			for r, err = tty.ReadRune(); err != nil || (r != 'a' && r != 's' && r != 'd'); r, err = tty.ReadRune() {
			}
		} else {
			if ballX < paddleX {
				r = 'a'
			} else if ballX > paddleX {
				r = 'd'
			} else {
				r = 's'
			}
		}
		switch r {
		case 'a':
			program.AppendInput(LEFT)
		case 's':
			program.AppendInput(NEUTRAL)
		case 'd':
			program.AppendInput(RIGHT)
		}
		program.ClearOutput()
		program.Execute()
		output := program.GetOutput()
		for i := 0; i < len(output); i += 3 {
			x := output[i]
			y := output[i+1]
			id := output[i+2]
			if x == -1 && y == 0 {
				score = id
				continue
			}
			switch id {
			case EMPTY_ID:
				gameMap[y][x] = EMPTY
			case WALL_ID:
				gameMap[y][x] = WALL
			case BLOCK_ID:
				gameMap[y][x] = BLOCK
				blocks++
			case HORIZONTAL_PADDLE_ID:
				gameMap[y][x] = HORIZONTAL_PADDLE
				paddleX = x
			case BALL_ID:
				gameMap[y][x] = BALL
				ballX = x
			}
		}
	}
	return score, blocks
}

func main() {
	input := common.ParseIntcodeProgram(common.Read("./inputs/example.txt"))
	score, blocks := playGame(input, false)
	fmt.Printf("Part 1: %v\n", blocks)
	fmt.Printf("Part 2: %v\n", score)
}
