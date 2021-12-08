package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strings"
)

type SignalPattern struct {
	in  []int
	out []int
}

func main() {
	patterns, err := ReadSignalWires("input")
	if err != nil {
		log.Fatalln(err)
	}
	fmt.Println("1:", CountUnique(patterns))
	fmt.Println("2:", DecodeSignals(patterns))
}

func CountUnique(patterns []SignalPattern) (count int) {
	for _, sp := range patterns {
		for _, ov := range sp.out {
			switch BitLen(ov) {
			case 2, 3, 4, 7:
				count++
			}
		}
	}
	return
}

func DecodeSignals(patterns []SignalPattern) (ans int) {
	for _, sp := range patterns {
		mappings := make([]int, 10)
		// Uniques:
		//     '1' is the only one with length 2
		//     '7' is the only one with length 3
		//     '4' is the only one with length 4
		//     '8' is the only one with length 7
		for i := 0; i < len(sp.in); {
			switch BitLen(sp.in[i]) {
			case 2:
				sp.in = MoveValue(sp.in, mappings, i, 1)
			case 3:
				sp.in = MoveValue(sp.in, mappings, i, 7)
			case 4:
				sp.in = MoveValue(sp.in, mappings, i, 4)
			case 7:
				sp.in = MoveValue(sp.in, mappings, i, 8)
			default:
				i++
			}
		}
		// Length == 6:
		//     '9' is the only one of which '4' is a perfect subset.
		//     '6' is the only one of which '1' is _not_ a perfect subset
		//     '0' is the only one left
		for i := 0; i < len(sp.in); {
			if BitLen(sp.in[i]) != 6 {
				i++
				continue
			}
			if Contains(sp.in[i], mappings[4]) {
				sp.in = MoveValue(sp.in, mappings, i, 9)
			} else if !Contains(sp.in[i], mappings[1]) {
				sp.in = MoveValue(sp.in, mappings, i, 6)
			} else {
				sp.in = MoveValue(sp.in, mappings, i, 0)
			}
		}
		// Length == 5:
		//     '5' is the only one that is a perfect subset of '6'
		//     '3' is the only one of which '1' is a perfect subset
		//     '2' is the only one left
		for i := 0; i < len(sp.in); {
			if BitLen(sp.in[i]) != 5 {
				i++
				continue
			}
			if Contains(mappings[6], sp.in[i]) {
				sp.in = MoveValue(sp.in, mappings, i, 5)
			} else if Contains(sp.in[i], mappings[1]) {
				sp.in = MoveValue(sp.in, mappings, i, 3)
			} else {
				sp.in = MoveValue(sp.in, mappings, i, 2)
			}
		}
		decodedNumber := 0
		for _, ov := range sp.out {
			decodedNumber *= 10
			for i, v := range mappings {
				if v == ov {
					decodedNumber += i
					break
				}
			}
		}
		ans += decodedNumber
	}
	return
}

// Copies val src[srcIndex] → dst[dstIndex], deletes it from src, returns new src
func MoveValue(src, dst []int, srcIndex, dstIndex int) []int {
	dst[dstIndex] = src[srcIndex]
	return append(src[:srcIndex], src[srcIndex+1:]...)
}

// Returns number of bits set in b
func BitLen(b int) (count int) {
	for b > 0 {
		count += b & 1
		b >>= 1
	}
	return
}

// Returns whether the bits in b2 are a subset (or equal) of the bits in b1
func Contains(b1, b2 int) bool {
	return (b1 & b2) == b2
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
		sp := SignalPattern{
			in:  make([]int, 10),
			out: make([]int, 4),
		}
		for i, v := range strings.Fields(parts[0]) {
			for _, c := range v {
				sp.in[i] += 1 << int(c-'a')
			}
		}
		for i, v := range strings.Fields(parts[1]) {
			for _, c := range v {
				sp.out[i] += 1 << int(c-'a')
			}
		}
		patterns = append(patterns, sp)
	}
	if err := scanner.Err(); err != nil {
		return nil, err
	}
	return patterns, nil
}
