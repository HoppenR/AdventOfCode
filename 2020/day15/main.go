package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

func main() {
	start, err := ReadStartNumbers("input")
	if err != nil {
		log.Fatalln(err)
	}
	fmt.Println("1:", MemoryGame(start, 2020))
	fmt.Println("2:", MemoryGame(start, 30000000))
}

func MemoryGame(start []int, turn int) int {
	spokenlast := make(map[int]int)
	for i, v := range start[:len(start)-2] {
		spokenlast[v] = i + 1
	}
	last := start[len(start)-1]
	lastlast := start[len(start)-2]
	for i := len(start) + 1; i <= turn; i++ {
		spokenlast[lastlast] = i - 2
		lastlast = last
		if age, ok := spokenlast[last]; ok {
			last = i - 1 - age
		} else {
			last = 0
		}
	}
	return last
}

func ReadStartNumbers(filename string) ([]int, error) {
	file, err := os.Open(filename)
	if err != nil {
		return nil, err
	}
	defer file.Close()
	scanner := bufio.NewScanner(file)
	numbers := make([]int, 0)
	scanner.Scan()
	for _, v := range strings.Split(scanner.Text(), ",") {
		num, err := strconv.Atoi(v)
		if err != nil {
			return nil, err
		}
		numbers = append(numbers, num)
	}
	if err := scanner.Err(); err != nil {
		return nil, err
	}
	return numbers, nil
}
