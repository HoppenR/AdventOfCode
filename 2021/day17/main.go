package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"math"
)

type Pos struct {
	x, y int
}

type Area struct {
	min Pos
	max Pos
}

func main() {
	area, err := GetArea("input")
	if err != nil {
		log.Fatalln(err)
	}
	fmt.Println("1:", CalculateFiring(area, 1))
	fmt.Println("2:", CalculateFiring(area, 2))
}

func CalculateFiring(area *Area, mode int) int {
	var ret int
	for xv := 1; xv <= 300; xv++ {
		for yv := -300; yv <= 300; yv++ {
			if mode == 1 {
				if hit, highY := Fire(xv, yv, area); hit && highY > ret {
					ret = highY
				}
			} else {
				if hit, _ := Fire(xv, yv, area); hit {
					ret++
				}
			}
		}
	}
	return ret
}

func Fire(xv, yv int, area *Area) (bool, int) {
	var pos Pos
	var highY int
	for i := 1; i <= 300; i++ {
		if pos.x >= area.min.x &&
			pos.x <= area.max.x &&
			pos.y >= area.min.y &&
			pos.y <= area.max.y {
			return true, highY
		}
		if pos.y < area.min.y {
			return false, 0
		}
		if pos.y > highY {
			highY = pos.y
		}
		pos.x += xv
		pos.y += yv
		if xv != 0 {
			xv -= xv / int(math.Abs(float64(xv)))
		}
		yv--
	}
	return false, 0
}

func GetArea(filename string) (*Area, error) {
	content, err := ioutil.ReadFile(filename)
	if err != nil {
		return nil, err
	}
	var minX, maxX, minY, maxY int
	_, err = fmt.Sscanf(
		string(content),
		"target area: x=%d..%d, y=%d..%d\n",
		&minX,
		&maxX,
		&minY,
		&maxY,
	)
	if err != nil {
		return nil, err
	}
	return &Area{Pos{minX, minY}, Pos{maxX, maxY}}, nil
}
