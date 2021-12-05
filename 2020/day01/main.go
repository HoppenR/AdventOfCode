package main

import (
	"bufio"
	"errors"
	"fmt"
	"log"
	"os"
	"strconv"
)

func main() {
	expenses, err := ReadPayments("input")
	if err != nil {
		log.Fatalln(err)
	}
	ans1, err := FindEntries(expenses, 2, 2020)
	if err != nil {
		log.Fatalln(err)
	}
	fmt.Println("1:", ans1)
	ans2, err := FindEntries(expenses, 3, 2020)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println("2:", ans2)
}

func ReadPayments(filename string) ([]int, error) {
	file, err := os.Open(filename)
	if err != nil {
		return nil, err
	}
	defer file.Close()
	scanner := bufio.NewScanner(file)
	expenses := make([]int, 0)
	for scanner.Scan() {
		payment, err := strconv.Atoi(scanner.Text())
		if err != nil {
			return nil, err
		}
		expenses = append(expenses, payment)
	}
	if err := scanner.Err(); err != nil {
		return nil, err
	}
	return expenses, nil
}

func FindEntries(expenses []int, nints, req int) (int, error) {
	factorIxs := make([]int, nints)
	for {
		if Sum(factorIxs, expenses) != req {
			if !AdvanceIxs(factorIxs, len(expenses)) {
				return 0, errors.New("Could not find numbers that sum up to 2020")
			}
		} else {
			var sum int = 1
			for i := 0; i < len(factorIxs); i++ {
				sum *= expenses[factorIxs[i]]
			}
			return sum, nil
		}
	}
}

// Returns true if it was able to increase a number
// starts at ixs[0] until it reaches NINTS, then increases ixs[1] by one and
// starts over until ixs[0] is full again, and so on
func AdvanceIxs(ixs []int, lines int) bool {
	var shouldadd bool = true
	for i := 0; i < len(ixs); i++ {
		if ixs[i] < lines && shouldadd {
			ixs[i]++
			shouldadd = false
		}
		if ixs[i] >= lines {
			if i == len(ixs)-1 {
				return false
			}
			ixs[i] = 0
			shouldadd = true
		}
	}
	return true
}

func Sum(ixs []int, expenses []int) (sum int) {
	for i := 0; i < len(ixs); i++ {
		sum += expenses[ixs[i]]
	}
	return sum
}
