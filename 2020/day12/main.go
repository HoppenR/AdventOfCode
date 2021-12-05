package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"strconv"
	"strings"
)

type Instr struct {
	action rune
	value  int
}

type Pos struct {
	x int
	y int
}

func main() {
	instructions, err := ReadLines("input")
	if err != nil {
		log.Fatalln(err)
	}
	fmt.Println("1:", Traverse(instructions))
	fmt.Println("2:", MoveWaypoint(instructions))
}

var Directions = map[int]rune{
	0: 'E',
	1: 'S',
	2: 'W',
	3: 'N',
}

var Movement = map[rune]Pos{
	'E': {1, 0},
	'S': {0, -1},
	'W': {-1, 0},
	'N': {0, 1},
}

func Traverse(instructions []Instr) int {
	var direction int
	var pos Pos
	for _, in := range instructions {
		switch in.action {
		case 'E', 'S', 'W', 'N':
			pos.x += Movement[in.action].x * in.value
			pos.y += Movement[in.action].y * in.value
		case 'F':
			pos.x += Movement[Directions[direction]].x * in.value
			pos.y += Movement[Directions[direction]].y * in.value
		case 'R':
			direction += in.value / 90
			direction = (direction + 4) % 4
		case 'L':
			direction -= in.value / 90
			direction = (direction + 4) % 4
		}
	}
	return IntAbs(pos.x) + IntAbs(pos.y)
}

func MoveWaypoint(instructions []Instr) int {
	var wp = Pos{10, 1}
	var pos Pos
	for _, in := range instructions {
		switch in.action {
		case 'E', 'S', 'W', 'N':
			wp.x += Movement[in.action].x * in.value
			wp.y += Movement[in.action].y * in.value
		case 'F':
			pos.x += wp.x * in.value
			pos.y += wp.y * in.value
		case 'R':
			for i := 0; i < in.value/90; i++ {
				wp.x, wp.y = wp.y, -wp.x
			}
		case 'L':
			for i := 0; i < in.value/90; i++ {
				wp.x, wp.y = -wp.y, wp.x
			}
		}
	}
	return IntAbs(pos.x) + IntAbs(pos.y)
}

func IntAbs(n int) int {
	if n < 0 {
		return -n
	} else {
		return n
	}
}

func ReadLines(filename string) ([]Instr, error) {
	content, err := ioutil.ReadFile(filename)
	if err != nil {
		return nil, err
	}
	trimcontent := strings.TrimRight(string(content), "\n")
	instructions := strings.Split(trimcontent, "\n")
	var prg = make([]Instr, 0)
	for _, l := range instructions {
		val, err := strconv.Atoi(l[1:])
		if err != nil {
			return nil, err
		}
		var in = Instr{rune(l[0]), val}
		prg = append(prg, in)
	}
	return prg, nil
}
