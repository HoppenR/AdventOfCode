package main

import (
	"bufio"
	"fmt"
	"log"
	"math"
	"os"
	"strings"
)

func main() {
	counts, rules, err := ParseCountsAndRules("input")
	if err != nil {
		log.Fatalln(err)
	}
	fmt.Println("1:", StepN(counts, rules, 10))
	fmt.Println("2:", StepN(counts, rules, 40))
}

func StepN(counts map[string]int, rules map[string]string, n int) int {
	for i := 0; i < n; i++ {
		counts = Step(counts, rules)
	}
	min, max := MinMaxFromCounts(counts)
	return max - min
}

func Step(counts map[string]int, rules map[string]string) map[string]int {
	newCounts := make(map[string]int)
	for pair, cnt := range counts {
		newCounts[pair[0:1]+rules[pair]] += cnt
		newCounts[rules[pair]+pair[1:2]] += cnt
	}
	return newCounts
}

func MinMaxFromCounts(counts map[string]int) (int, int) {
	charCountsX2 := make(map[byte]int)
	for pair, cnt := range counts {
		charCountsX2[pair[0]] += cnt
		charCountsX2[pair[1]] += cnt
	}
	min := math.MaxFloat64
	max := 0.0
	for _, charCountX2 := range charCountsX2 {
		charCount := math.Ceil(float64(charCountX2) / 2)
		max = math.Max(max, charCount)
		min = math.Min(min, charCount)
	}
	return int(min), int(max)
}

func ParseCountsAndRules(filename string) (map[string]int, map[string]string, error) {
	file, err := os.Open(filename)
	if err != nil {
		return nil, nil, err
	}
	defer file.Close()
	scanner := bufio.NewScanner(file)
	scanner.Scan()
	counts := make(map[string]int)
	for i := 0; i < len(scanner.Text())-1; i++ {
		counts[scanner.Text()[i:i+2]]++
	}
	scanner.Scan()
	rules := make(map[string]string)
	for scanner.Scan() {
		parts := strings.Split(scanner.Text(), " -> ")
		rules[parts[0]] = parts[1]
	}
	if err := scanner.Err(); err != nil {
		return nil, nil, err
	}
	return counts, rules, nil
}
