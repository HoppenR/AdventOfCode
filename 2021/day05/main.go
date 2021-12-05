package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
)

func main() {
	segments, err := ReadSegments("input")
	if err != nil {
		log.Fatalln(err)
	}
	fmt.Println("1:", CountOverlap(segments, false))
	fmt.Println("2:", CountOverlap(segments, true))
}

func CountOverlap(segments map[string]int, useDiag bool) (ctr int) {
	for _, v := range segments {
		if !useDiag && (v%1000 >= 2) || useDiag && (v/1000+v%1000 >= 2) {
			ctr++
		}
	}
	return
}

func ReadSegments(filename string) (map[string]int, error) {
	file, err := os.Open(filename)
	if err != nil {
		return nil, err
	}
	defer file.Close()
	scanner := bufio.NewScanner(file)
	segments := make(map[string]int)
	for scanner.Scan() {
		var x1, y1, x2, y2, Δx, Δy int
		_, err := fmt.Sscanf(scanner.Text(), "%d,%d -> %d,%d", &x1, &y1, &x2, &y2)
		if err != nil {
			return nil, err
		}
		if x1 < x2 {
			Δx = 1
		} else if x1 > x2 {
			Δx = -1
		}
		if y1 < y2 {
			Δy = 1
		} else if y1 > y2 {
			Δy = -1
		}
		for x, y := x1, y1; x != (x2+Δx) || y != (y2+Δy); x, y = x+Δx, y+Δy {
			if Δx == 0 || Δy == 0 {
				segments[fmt.Sprintf("%d,%d", x, y)] += 1
			} else {
				segments[fmt.Sprintf("%d,%d", x, y)] += 1000
			}
		}
	}
	if err := scanner.Err(); err != nil {
		return nil, err
	}
	return segments, nil
}
