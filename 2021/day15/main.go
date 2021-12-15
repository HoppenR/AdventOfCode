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

var compOffsets = []Point{
	{1, 0}, {-1, 0}, {0, 1}, {0, -1},
}

func main() {
	costs, err := LoadGrid("input")
	if err != nil {
		log.Fatalln(err)
	}
	fmt.Println("1:", ShortestPath(costs))
}

func ShortestPath(costs map[Point]int) int {
	unvisited := make(map[Point]struct{})
	for k := range costs {
		unvisited[k] = struct{}{}
	}
	SPT := make(map[Point]int)
	SPT[Point{0, 0}] = 0
	for len(SPT) < len(costs) {
		shortestPath := math.MaxInt
		var cur Point
		for k, v := range SPT {
			if _, ok := unvisited[k]; !ok {
				continue
			}
			if v < shortestPath {
				cur = k
				shortestPath = v
			}
		}
		for _, Δc := range compOffsets {
			comparison := Point{cur.x + Δc.x, cur.y + Δc.y}
			if _, ok := costs[comparison]; !ok {
				continue
			}
			if _, ok := unvisited[comparison]; !ok {
				continue
			}
			prevBest, ok := SPT[comparison]
			if shortestPath+costs[comparison] < prevBest || !ok {
				SPT[comparison] = shortestPath + costs[comparison]
			}
		}
		delete(unvisited, cur)
	}
	sideLen := int(math.Sqrt(float64(len(SPT))))
	return SPT[Point{sideLen - 1, sideLen - 1}]
}

func LoadGrid(filename string) (map[Point]int, error) {
	content, err := ioutil.ReadFile(filename)
	if err != nil {
		return nil, err
	}
	costs := make(map[Point]int)
	for y, row := range bytes.Split(content, []byte("\n")) {
		for x, col := range row {
			costs[Point{x, y}] = int(col - '0')
		}
	}
	return costs, nil
}
