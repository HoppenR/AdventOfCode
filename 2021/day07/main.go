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
	positions, err := LoadPositions("input")
	if err != nil {
		log.Fatalln(err)
	}
	fmt.Println("1:", FindBest(positions, false))
	fmt.Println("2:", FindBest(positions, true))
}

func FindBest(positions []int, fuelIncrease bool) int {
	max := 0
	for _, v := range positions {
		if v > max {
			max = v
		}
	}
	best := math.MaxInt64
	for goal := 0; goal < max; goal++ {
		cur := 0
		for _, v := range positions {
			if !fuelIncrease {
				cur += IntAbs(goal - v)
			} else {
				cur += SumZeroToN(IntAbs(goal - v))
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

func LoadPositions(filename string) ([]int, error) {
	content, err := ioutil.ReadFile(filename)
	if err != nil {
		return nil, err
	}
	trimcontent := bytes.TrimRight(content, "\n")
	var positions []int
	for _, l := range bytes.Split(trimcontent, []byte(",")) {
		num, err := strconv.Atoi(string(l))
		if err != nil {
			return nil, err
		}
		positions = append(positions, num)
	}
	return positions, nil
}
