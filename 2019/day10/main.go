package main

import (
	"common"
	"fmt"
	"math"
	"sort"
	"strings"
)

type station struct {
	row, col int
}

func gcd(a, b int) int {
	if b == 0 {
		return a
	}
	if a == 0 {
		return b
	}
	return gcd(b, a%b)
}

func abs(a int) int {
	if a < 0 {
		return -a
	}
	return a
}

func findBestStation(stationMap [][]byte, stations []station) ([]station, station) {
	var bestStation station
	var maxDetected []station
	for i := 0; i < len(stations); i++ {
		detected := make([]station, 0)
		for j := 0; j < len(stations); j++ {
			if i == j {
				continue
			}
			dr := stations[j].row - stations[i].row
			dc := stations[j].col - stations[i].col
			g := gcd(abs(dr), abs(dc))
			drStep := dr / g
			dcStep := dc / g
			row := stations[i].row
			col := stations[i].col
			for row != stations[j].row || col != stations[j].col {
				row += drStep
				col += dcStep
				if stationMap[row][col] == '#' {
					break
				}
			}
			if row == stations[j].row && col == stations[j].col {
				detected = append(detected, stations[j])
			}
		}
		if maxDetected == nil || len(detected) > len(maxDetected) {
			maxDetected = detected
			bestStation = stations[i]
		}
	}
	return maxDetected, bestStation
}

func quartal(row, col int) int {
	if row >= 0 && col >= 0 {
		return 0
	} else if row <= 0 && col >= 0 {
		return 1
	} else if row <= 0 && col <= 0 {
		return 2
	} else {
		return 3
	}
}

func find200thDestroyedAsteroid(laser station, detected []station) int {
	sort.SliceStable(detected, func(i, j int) bool {
		dri := laser.row - detected[i].row
		dci := detected[i].col - laser.col
		drj := laser.row - detected[j].row
		dcj := detected[j].col - laser.col
		if quartal(dri, dci) < quartal(drj, dcj) {
			return true
		}
		if quartal(dri, dci) > quartal(drj, dcj) {
			return false
		}
		anglei := math.Atan2(float64(dri), float64(dci))
		anglej := math.Atan2(float64(drj), float64(dcj))
		return anglei >= anglej
	})
	return detected[199].row + 100*detected[199].col
}

func parse(content string) ([][]byte, []station) {
	stationMap := make([][]byte, 0)
	stations := make([]station, 0)
	r := 0
	for l := range strings.Lines(content) {
		row := []byte(strings.TrimSpace(l))
		stationMap = append(stationMap, row)
		for c, char := range row {
			if char == '#' {
				station := station{
					row: r,
					col: c,
				}
				stations = append(stations, station)
			}
		}
		r++
	}
	return stationMap, stations
}

func main() {
	stationMap, stations := parse(common.Read("./inputs/example.txt"))
	detectedStations, bestStation := findBestStation(stationMap, stations)
	fmt.Printf("Part 1: %v\n", len(detectedStations))
	fmt.Printf("Part 2: %v\n", find200thDestroyedAsteroid(bestStation, detectedStations))
}
