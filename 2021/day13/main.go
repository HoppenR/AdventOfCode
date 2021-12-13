package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"strconv"
	"strings"
)

type Point struct {
	x xFold
	y yFold
}

type xFold int
type yFold int

func main() {
	points, folds, err := LoadData("input")
	if err != nil {
		log.Fatalln(err)
	}
	fmt.Println("1:", Fold(points, folds, 1))
	fmt.Println("2:", Fold(points, folds, -1))
}

func Fold(points map[Point]struct{}, folds []interface{}, nFolds int) string {
	for i, fld := range folds {
		if i == nFolds {
			return fmt.Sprint(len(points))
		}
		switch foldMid := fld.(type) {
		case xFold:
			for point := range points {
				if point.x > foldMid {
					points[Point{point.x - 2*(point.x-foldMid), point.y}] = struct{}{}
					delete(points, point)
				}
			}
		case yFold:
			for point := range points {
				if point.y > foldMid {
					points[Point{point.x, point.y - 2*(point.y-foldMid)}] = struct{}{}
					delete(points, point)
				}
			}
		}
	}
	return PrintLetters(points)
}

func PrintLetters(points map[Point]struct{}) string {
	var maxX xFold
	var maxY yFold
	for k := range points {
		if k.x > maxX {
			maxX = k.x
		}
		if k.y > maxY {
			maxY = k.y
		}
	}
	bldr := strings.Builder{}
	for y := yFold(0); y <= maxY; y++ {
		bldr.WriteRune('\n')
		for x := xFold(0); x <= maxX; x++ {
			if _, ok := points[Point{x, y}]; ok {
				bldr.WriteRune('#')
			} else {
				bldr.WriteRune(' ')
			}
		}
	}
	return bldr.String()
}

func LoadData(filename string) (map[Point]struct{}, []interface{}, error) {
	content, err := ioutil.ReadFile(filename)
	if err != nil {
		return nil, nil, err
	}
	trimcontent := strings.TrimRight(string(content), "\n")
	parts := strings.Split(trimcontent, "\n\n")
	points := make(map[Point]struct{})
	for _, line := range strings.Split(parts[0], "\n") {
		var x xFold
		var y yFold
		fmt.Sscanf(line, "%d,%d\n", &x, &y)
		points[Point{x, y}] = struct{}{}
	}
	var folds []interface{}
	for _, line := range strings.Split(parts[1], "\n") {
		instrParts := strings.Split(strings.TrimPrefix(line, "fold along "), "=")
		value, err := strconv.Atoi(instrParts[1])
		if err != nil {
			return nil, nil, err
		}
		switch instrParts[0] {
		case "x":
			folds = append(folds, xFold(value))
		case "y":
			folds = append(folds, yFold(value))
		}
	}
	return points, folds, nil
}
