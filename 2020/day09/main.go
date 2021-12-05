package main

import (
	"bufio"
	"errors"
	"fmt"
	"log"
	"math"
	"os"
	"strconv"
)

func main() {
	data, err := ReadXMASData("input")
	if err != nil {
		log.Fatalln(err)
	}
	ans1, err := FindNonSum(data, 25)
	if err != nil {
		log.Fatalln(err)
	}
	fmt.Println("1:", ans1)
	ans2, err := FindSubset(data, ans1)
	if err != nil {
		log.Fatalln(err)
	}
	fmt.Println("2:", ans2)
}

// Finds the first number which does not have two numbers in its preamble that
// adds up to it
func FindNonSum(set []int, preambleSz int) (int, error) {
	for i, v := range set[preambleSz:] {
		if !FindInPerms(set[i:i+preambleSz], v) {
			return v, nil
		}
	}
	return 0, errors.New("All numbers have a preamble that adds up to them")
}

// Finds the first contiguous set of numbers that add up to target, then returns
// the maximum and minimum number in that contiguous range
func FindSubset(set []int, target int) (int, error) {
	var start, end, sum int = 0, 0, set[0]
	for end < len(set) {
		if sum > target {
			sum -= set[start]
			start++
		} else if sum < target {
			end++
			sum += set[end]
		} else {
			min, max := FindExtremes(set[start : end+1])
			return min + max, nil
		}
	}
	return 0, errors.New("Couldn't find a set that adds to target")
}

func FindExtremes(set []int) (int, int) {
	var min int = math.MaxInt64
	var max int = 0
	for _, v := range set {
		if v < min {
			min = v
		}
		if v > max {
			max = v
		}
	}
	return min, max
}

// Returns true if any two other numbers in set add up to n
func FindInPerms(set []int, n int) bool {
	for i, v := range set {
		for j := i + 1; j < len(set); j++ {
			if v+set[j] == n {
				return true
			}
		}
	}
	return false
}

func ReadXMASData(filename string) ([]int, error) {
	file, err := os.Open(filename)
	if err != nil {
		return nil, err
	}
	defer file.Close()
	scanner := bufio.NewScanner(file)
	XMASData := make([]int, 0)
	for scanner.Scan() {
		number, err := strconv.Atoi(scanner.Text())
		if err != nil {
			return nil, err
		}
		XMASData = append(XMASData, number)
	}
	if err := scanner.Err(); err != nil {
		return nil, err
	}
	return XMASData, nil
}
