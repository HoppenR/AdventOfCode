package main

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"log"
	"math"
)

type Point struct {
	x, y int
}

type Octopus struct {
	energy  int
	flashed bool
}

var compOffsets = []Point{
	{1, 0}, {-1, 0}, {0, 1}, {0, -1}, {-1, 1}, {1, 1}, {-1, -1}, {1, -1},
}

func main() {
	grid, err := LoadGrid("input")
	if err != nil {
		log.Fatalln(err)
	}
	fmt.Println("1:", SimulateGrid(grid, 100, false))
	fmt.Println("2:", SimulateGrid(grid, math.MaxInt, true))
}

func SimulateGrid(origGrid map[Point]*Octopus, steps int, simul bool) int {
	totalFlashes := 0
	grid := make(map[Point]*Octopus)
	for k, v := range origGrid {
		grid[k] = &Octopus{v.energy, v.flashed}
	}
	for i := 1; i <= steps; i++ {
		for point := range grid {
			grid[point].energy++
			grid[point].flashed = false
		}
		for {
			increased := false
			for point, octo := range grid {
				if octo.flashed {
					continue
				}
				if octo.energy > 9 {
					octo.flashed = true
					for _, Δc := range compOffsets {
						if _, ok := grid[Point{point.x + Δc.x, point.y + Δc.y}]; ok {
							grid[Point{point.x + Δc.x, point.y + Δc.y}].energy++
							increased = true
						}
					}
				}
			}
			if !increased {
				break
			}
		}
		stepFlashes := 0
		for point, octo := range grid {
			if octo.energy > 9 {
				grid[point].energy = 0
				stepFlashes++
			}
		}
		if simul && stepFlashes == 100 {
			return i
		}
		totalFlashes += stepFlashes
	}
	return totalFlashes
}

func LoadGrid(filename string) (map[Point]*Octopus, error) {
	content, err := ioutil.ReadFile(filename)
	if err != nil {
		return nil, err
	}
	points := make(map[Point]*Octopus)
	for y, row := range bytes.Split(content, []byte("\n")) {
		for x, col := range row {
			points[Point{x, y}] = &Octopus{int(col - '0'), false}
		}
	}
	return points, nil
}
