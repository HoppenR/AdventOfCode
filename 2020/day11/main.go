package main

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"log"
	"strings"
)

type Point struct {
	r int
	c int
}

var Directions = []Point{
	{-1, -1}, {0, -1}, {1, -1},
	{-1, 0}, {1, 0},
	{-1, 1}, {0, 1}, {1, 1},
}

func main() {
	seats, err := ReadSeats("input")
	if err != nil {
		log.Fatalln(err)
	}
	fmt.Println("1:", FindStagnation(seats, 1))
	fmt.Println("2:", FindStagnation(seats, 2))
}

func FindStagnation(boardC [][]byte, ruleset int) (count int) {
	defer ResetOccupied(boardC)
	boardN := make([][]byte, len(boardC))
	for i := range boardN {
		boardN[i] = make([]byte, len(boardC[0]))
	}
	for NextState(boardC, boardN, ruleset) {
		boardC, boardN = boardN, boardC
	}
	for i := range boardC {
		count += bytes.Count(boardC[i], []byte{'#'})
	}
	return
}

func NextState(boardC, boardN [][]byte, ruleset int) (changed bool) {
	for i := range boardC {
		for j := range boardC[i] {
			if boardC[i][j] == '.' || boardC[i][j] == 0x0 {
				continue
			}
			occ := AdjOccupied(boardC, Point{i, j}, ruleset)
			if boardC[i][j] == 'L' && occ == 0 {
				boardN[i][j] = '#'
				changed = true
			} else if boardC[i][j] == '#' && occ >= 3+ruleset {
				boardN[i][j] = 'L'
				changed = true
			} else {
				boardN[i][j] = boardC[i][j]
			}
		}
	}
	return
}

func AdjOccupied(seats [][]byte, p Point, ruleset int) (occupied int) {
	for _, d := range Directions {
		if (p.r+d.r < 0 || p.r+d.r >= len(seats)) ||
			(p.c+d.c < 0 || p.c+d.c >= len(seats[0])) {
			continue
		}
		if ruleset == 1 {
			if seats[p.r+d.r][p.c+d.c] == '#' {
				occupied++
			}
		}
		if ruleset == 2 {
			var offset Point = d
			for {
				if strings.IndexByte("#L", seats[p.r+offset.r][p.c+offset.c]) >= 0 {
					break
				}
				if (p.r+offset.r+d.r >= 0 && p.r+offset.r+d.r < len(seats)) &&
					(p.c+offset.c+d.c >= 0 && p.c+offset.c+d.c < len(seats[0])) {
					offset.r += d.r
					offset.c += d.c
				} else {
					break
				}
			}
			if seats[p.r+offset.r][p.c+offset.c] == '#' {
				occupied++
			}
		}
	}
	return
}

func ResetOccupied(seats [][]byte) {
	for i := range seats {
		seats[i] = bytes.ReplaceAll(seats[i], []byte{'#'}, []byte{'L'})
	}
}

func ReadSeats(filename string) ([][]byte, error) {
	content, err := ioutil.ReadFile(filename)
	if err != nil {
		return nil, err
	}
	trimmed := bytes.TrimSpace(content)
	seats := bytes.Split(trimmed, []byte{'\n'})
	return seats, nil
}
