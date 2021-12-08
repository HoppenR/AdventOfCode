package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strings"
)

type SignalPattern struct {
	input  [10]string
	output [4]string
}

func main() {
	patterns, err := ReadSignalWires("input")
	if err != nil {
		log.Fatalln(err)
	}
	fmt.Println("1:", CountUnique(patterns))
}

func CountUnique(patterns []SignalPattern) (count int) {
	for _, sp := range patterns {
		for _, ov := range sp.output {
			switch len(ov) {
			case 2, 3, 4, 7: // '1', '7', '4', '8'
				count++
			}
		}
	}
	return
}

func ReadSignalWires(filename string) ([]SignalPattern, error) {
	file, err := os.Open(filename)
	if err != nil {
		return nil, err
	}
	defer file.Close()
	scanner := bufio.NewScanner(file)
	var patterns []SignalPattern
	for scanner.Scan() {
		parts := strings.Split(scanner.Text(), "|")
		var sp SignalPattern
		for i, w := range strings.Fields(parts[0]) {
			sp.input[i] = w
		}
		for i, w := range strings.Fields(parts[1]) {
			sp.output[i] = w
		}
		patterns = append(patterns, sp)
	}
	if err := scanner.Err(); err != nil {
		return nil, err
	}
	return patterns, nil
}
