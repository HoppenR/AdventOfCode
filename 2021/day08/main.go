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
		numToSig := make([]int, 10)
		for _, sig := range sp.in {
			switch BitLen(sig) {
			case 2:
				numToSig[1] = sig
			case 3:
				numToSig[7] = sig
			case 4:
				numToSig[4] = sig
			case 7:
				numToSig[8] = sig
			}
		}
		for _, sig := range sp.in {
			if BitLen(sig) != 6 {
				continue
			}
			if Contains(sig, numToSig[4]) {
				numToSig[9] = sig
			} else if !Contains(sig, numToSig[1]) {
				numToSig[6] = sig
			} else {
				numToSig[0] = sig
			}
		}
		for _, sig := range sp.in {
			if BitLen(sig) != 5 {
				continue
			}
			if Contains(numToSig[6], sig) {
				numToSig[5] = sig
			} else if Contains(sig, numToSig[1]) {
				numToSig[3] = sig
			} else {
				numToSig[2] = sig
			}
		}
		decodedNumber := 0
		for _, output := range sp.out {
			decodedNumber *= 10
			for num, signal := range numToSig {
				if signal == output {
					decodedNumber += num
					break
				}
			}
		}
		ans += decodedNumber
	}
	return
}

// Returns number of bits set in b
func BitLen(b int) (count int) {
	for n := b; n > 0; n >>= 1 {
		count += n & 1
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
