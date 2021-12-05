package main

import (
	"fmt"
	"io/ioutil"
	"log"
)

type Coord struct {
	x int
	y int
}

// Marks all colored tiles
type Floor = map[Coord]struct{}

func main() {
	movement, err := ReadLabels("input")
	if err != nil {
		log.Fatalln(err)
	}
	fmt.Println("1:", CountPainted(movement, false))
	fmt.Println("2:", CountPainted(movement, true))
}

func CountPainted(movement []Coord, part2 bool) int {
	if !part2 {
		return len(Paint(movement))
	} else {
		return len(TileOfLife(movement, 100))
	}
}

func TileOfLife(movement []Coord, iterations int) Floor {
	floor := Paint(movement)
	for i := 0; i < iterations; i++ {
		floor = FlipTiles(floor)
	}
	return floor
}

func FlipTiles(floorC Floor) Floor {
	floorN := make(Floor)
	candidates := make(map[Coord]int)
	for t := range floorC {
		cnt := 0
		for _, c := range []Coord{{2, 0}, {1, 2}, {1, -2}, {-2, 0}, {-1, 2}, {-1, -2}} {
			check := Coord{t.x + c.x, t.y + c.y}
			if _, ok := floorC[check]; ok {
				cnt++
			}
			candidates[check]++
		}
		if cnt == 1 || cnt == 2 {
			floorN[t] = struct{}{}
		}
	}
	for k, v := range candidates {
		if v == 2 {
			floorN[k] = struct{}{}
		}
	}
	return floorN
}

func Paint(movement []Coord) Floor {
	painted := make(Floor)
	var pos = Coord{0, 0}
	for _, v := range movement {
		pos.x += v.x
		pos.y += v.y
		if v == (Coord{0, 0}) {
			// A movement of {0, 0} means paint and reset pos
			if _, ok := painted[pos]; !ok {
				painted[pos] = struct{}{}
			} else {
				delete(painted, pos)
			}
			pos = v
		}
	}
	return painted
}

func ReadLabels(filename string) (movement []Coord,  err error) {
	content, err := ioutil.ReadFile(filename)
	if err != nil {
		return
	}
	for i := 0; i < len(content); i++ {
		switch content[i] {
		case 'e':
			movement = append(movement, Coord{2, 0})
		case 'w':
			movement = append(movement, Coord{-2, 0})
		case 'n':
			i++
			switch content[i] {
			case 'e':
				movement = append(movement, Coord{1, 2})
			case 'w':
				movement = append(movement, Coord{-1, 2})
			}
		case 's':
			i++
			switch content[i] {
			case 'e':
				movement = append(movement, Coord{1, -2})
			case 'w':
				movement = append(movement, Coord{-1, -2})
			}
		case '\n':
			// A movement of {0, 0} means paint and reset pos
			movement = append(movement, Coord{0, 0})
		}
	}
	return
}
