package main

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"log"
	"sort"
)

type Point struct {
	x, y int
}

var compOffsets = []Point{
	{1, 0}, {-1, 0}, {0, 1}, {0, -1},
}

func main() {
	points, err := LoadMap("input")
	if err != nil {
		log.Fatalln(err)
	}
	fmt.Println("1:", RiskLevel(points))
	fmt.Println("2:", LargeBasinScore(points))
}

func RiskLevel(points map[Point]int) (sum int) {
	for p, n := range points {
		if IsLocalMinima(points, p, n) {
			sum += n + 1
		}
	}
	return
}

func IsLocalMinima(points map[Point]int, p Point, n int) bool {
	for _, Δc := range compOffsets {
		if height, ok := points[p.Add(Δc)]; ok {
			if height <= n {
				return false
			}
		}
	}
	return true
}

func (lhs Point) Add(rhs Point) Point {
	lhs.x += rhs.x
	lhs.y += rhs.y
	return lhs
}

func LargeBasinScore(points map[Point]int) int {
	var sizes []int
	for p, n := range points {
		if IsLocalMinima(points, p, n) {
			sizes = append(sizes, BasinSize(points, p))
		}
	}
	sort.Sort(sort.Reverse(sort.IntSlice(sizes)))
	return sizes[0] * sizes[1] * sizes[2]
}

func BasinSize(points map[Point]int, lowPoint Point) (size int) {
	toCheck := map[Point]struct{}{
		lowPoint: {},
	}
	seen := make(map[Point]struct{})
	for len(toCheck) > 0 {
		for point := range toCheck {
			for _, Δc := range compOffsets {
				newPoint := point.Add(Δc)
				if height, ok := points[newPoint]; ok && height != 9 {
					if _, ok := seen[newPoint]; !ok {
						toCheck[newPoint] = struct{}{}
					}
				}
			}
			seen[point] = struct{}{}
			size++
			delete(toCheck, point)
		}
	}
	return
}

func LoadMap(filename string) (map[Point]int, error) {
	content, err := ioutil.ReadFile(filename)
	if err != nil {
		return nil, err
	}
	points := make(map[Point]int)
	for y, row := range bytes.Split(content, []byte("\n")) {
		for x, col := range row {
			points[Point{x, y}] = int(col - '0')
		}
	}
	return points, nil
}
