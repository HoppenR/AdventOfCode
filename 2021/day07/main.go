package main

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"log"
	"math"
	"strconv"
)

func main() {
	positions, max, err := LoadPositions("input")
	if err != nil {
		log.Fatalln(err)
	}
	fmt.Println("1:", FindBest(positions, max, false))
	fmt.Println("2:", FindBest(positions, max, true))
}

func FindBest(positions []int, max int, fuelIncrease bool) int {
	best := math.MaxInt64
	for meetPoint := 0; meetPoint < max; meetPoint++ {
		cur := 0
		for _, pos := range positions {
			if !fuelIncrease {
				cur += IntAbs(meetPoint - pos)
			} else {
				cur += SumZeroToN(IntAbs(meetPoint - pos))
			}
		}
		if cur < best {
			best = cur
		}
	}
	return best
}

func IntAbs(n int) int {
	if n < 0 {
		return -n
	}
	return n
}

func SumZeroToN(n int) int {
	return n * (n + 1) / 2
}

func LoadPositions(filename string) ([]int, int, error) {
	content, err := ioutil.ReadFile(filename)
	if err != nil {
		return nil, 0, err
	}
	trimcontent := bytes.TrimRight(content, "\n")
	var positions []int
	var max int
	for _, l := range bytes.Split(trimcontent, []byte(",")) {
		num, err := strconv.Atoi(string(l))
		if err != nil {
			return nil, 0, err
		}
		if num > max {
			max = num
		}
		positions = append(positions, num)
	}
	return positions, max, nil
}
