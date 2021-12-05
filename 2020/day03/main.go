package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"strings"
)

type Slope struct {
	x int
	y int
}

var SlopeTracks = []Slope{{1, 1}, {3, 1}, {5, 1}, {7, 1}, {1, 2}}

func main() {
	m, err := ReadTreeMap("input")
	if err != nil {
		log.Fatalln(err)
	}
	fmt.Println("1:", CountTrees(m, Slope{3, 1}))
	fmt.Println("2:", MultiplySlopes(m, SlopeTracks))
}

func MultiplySlopes(m []string, slopes []Slope) int {
	var cnt int = 1
	for _, s := range slopes {
		cnt *= CountTrees(m, s)
	}
	return cnt
}

func CountTrees(m []string, s Slope) int {
	lenX := len(m[0])
	lenY := len(m)
	var cnt int
	for x, y := 0, 0; y < lenY; x, y = (x+s.x)%lenX, y+s.y {
		if m[y][x] == '#' {
			cnt++
		}
	}
	return cnt
}

func ReadTreeMap(filename string) ([]string, error) {
	content, err := ioutil.ReadFile(filename)
	if err != nil {
		return nil, err
	}
	lines := strings.Split(string(content), "\n")
	return lines[:len(lines)-1], nil
}
