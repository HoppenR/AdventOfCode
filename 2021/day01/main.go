package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
)

func main() {
	depths, err := ReadDepths("input")
	if err != nil {
		log.Fatalln(err)
	}
	fmt.Println("1:", FindIncrementsWindow(depths, 1))
	fmt.Println("2:", FindIncrementsWindow(depths, 3))
}

func FindIncrementsWindow(depths []int, window int) (increments int) {
	lastMeasure := 0
	for i := 0; i < len(depths)-window; i++ {
		measure := 0
		for j := i; j < i + window; j++ {
			measure += depths[j]
		}
		if measure > lastMeasure {
			increments++
		}
		lastMeasure = measure
	}
	return
}

func ReadDepths(filename string) ([]int, error) {
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

