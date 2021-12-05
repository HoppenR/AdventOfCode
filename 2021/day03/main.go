package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
)

func main() {
	diagnostic, nBits, err := ReadDiagnostic("input")
	if err != nil {
		log.Fatalln(err)
	}
	fmt.Println("1:", PowerConsumption(diagnostic, nBits))
	fmt.Println("2:", LifeSupport(diagnostic, nBits))
}

func FilterBitCriteria(diagnostic []int64, nBits int, defaultValue int64) int64 {
	diagCpy := make([]int64, len(diagnostic))
	copy(diagCpy, diagnostic)
	for i := nBits - 1; i >= 0; i-- {
		var common [2]int
		for _, v := range diagCpy {
			common[(v>>i)%2]++
		}
		for j := 0; j < len(diagCpy); {
			if common[1] >= common[0] {
				if (diagCpy[j]>>i)%2 != defaultValue {
					diagCpy = append(diagCpy[:j], diagCpy[j+1:]...)
					continue
				}
			} else {
				if (diagCpy[j]>>i)%2 == defaultValue {
					diagCpy = append(diagCpy[:j], diagCpy[j+1:]...)
					continue
				}
			}
			j++
		}
		if len(diagCpy) == 1 {
			return diagCpy[0]
		}
	}
	panic("Unreachable")
}

func LifeSupport(diagnostic []int64, nBits int) int64 {
	return FilterBitCriteria(diagnostic, nBits, 1) * FilterBitCriteria(diagnostic, nBits, 0)
}

func PowerConsumption(diagnostic []int64, nBits int) int64 {
	var ε, γ int64
	for i := nBits - 1; i >= 0; i-- {
		var common [2]int
		for _, v := range diagnostic {
			common[(v>>i)%2]++
		}
		if common[1] > common[0] {
			ε += 1 << i
		} else {
			γ += 1 << i
		}
	}
	return ε * γ
}

func ReadDiagnostic(filename string) ([]int64, int, error) {
	file, err := os.Open(filename)
	if err != nil {
		return nil, 0, err
	}
	defer file.Close()
	scanner := bufio.NewScanner(file)
	diagnostic := make([]int64, 0)
	var nBits int
	for scanner.Scan() {
		nBits = len(scanner.Text())
		binaryNum, err := strconv.ParseInt(scanner.Text(), 2, 0)
		if err != nil {
			return nil, 0, err
		}
		diagnostic = append(diagnostic, binaryNum)
	}
	if err := scanner.Err(); err != nil {
		return nil, 0, err
	}
	return diagnostic, nBits, nil
}
