package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"sort"
	"strconv"
)

func main() {
	adapts, err := ReadJolts("input")
	if err != nil {
		log.Fatalln(err)
	}
	fmt.Println("1:", JoltGapValue(adapts))
	fmt.Println("2:", Combinations(adapts))
}

func JoltGapValue(adapts []int) int {
	diffs := make([]int, 4)
	for i := range adapts[:len(adapts)-1] {
		diffs[(adapts[i+1]-adapts[i])]++
	}
	return diffs[1] * diffs[3]
}

func Combinations(adapts []int) int {
	// combs[i] represents the number of ways to get from 0 to the ith number
	combs := make([]int, len(adapts))
	// There is 1 way to get to the first number
	combs[0] = 1
	for i := range adapts {
		// Add the ways to get to the previous ~3 numbers that are within reach
		// of adapts[i]
		for j := i - 1; j >= 0; j-- {
			if adapts[i]-adapts[j] > 3 {
				break
			}
			combs[i] += combs[j]
		}
	}
	return combs[len(combs)-1]
}

func ReadJolts(filename string) ([]int, error) {
	file, err := os.Open(filename)
	if err != nil {
		return nil, err
	}
	defer file.Close()
	scanner := bufio.NewScanner(file)
	adapts := make([]int, 0)
	for scanner.Scan() {
		jolt, err := strconv.Atoi(scanner.Text())
		if err != nil {
			return nil, err
		}
		adapts = append(adapts, jolt)
	}
	if err := scanner.Err(); err != nil {
		return nil, err
	}
	// Set up the outlet and device
	adapts = append(adapts, 0)
	sort.IntSlice.Sort(adapts)
	adapts = append(adapts, adapts[len(adapts)-1]+3)
	return adapts, nil
}
