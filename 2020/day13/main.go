package main

import (
	"bufio"
	"fmt"
	"log"
	"math"
	"os"
	"strconv"
	"strings"
)

func main() {
	departure, timetable, err := ReadTimetable("input")
	if err != nil {
		log.Fatalln(err)
	}
	fmt.Println("1:", EarliestBus(departure, timetable))
	fmt.Println("2:", DepartureSeriesTime(timetable))
}

func DepartureSeriesTime(timetable []interface{}) (t int) {
	busIDs := make([]int, 0)
	offsets := make([]int, 0)
	for i, in := range timetable {
		switch v := in.(type) {
		case int:
			busIDs = append(busIDs, v)
			offsets = append(offsets, i)
		}
	}
	jump := 1
	for i := range busIDs {
		for (t+offsets[i])%busIDs[i] != 0 {
			t += jump
		}
		jump *= busIDs[i]
	}
	return t
}

func EarliestBus(departure int, timetable []interface{}) (ans int) {
	firstBus := math.MaxInt64
	busID := 0
	for _, in := range timetable {
		switch v := in.(type) {
		case int:
			for i := departure; ; i++ {
				if i%v == 0 {
					if i < firstBus {
						firstBus = i
						busID = v
					}
					break
				}
			}
		}
	}
	return (firstBus - departure) * busID
}

func ReadTimetable(filename string) (int, []interface{}, error) {
	file, err := os.Open(filename)
	if err != nil {
		return 0, nil, err
	}
	defer file.Close()
	scanner := bufio.NewScanner(file)
	scanner.Scan()
	departure, err := strconv.Atoi(scanner.Text())
	if err != nil {
		return 0, nil, err
	}
	expenses := make([]interface{}, 0)
	scanner.Scan()
	for _, l := range strings.Split(scanner.Text(), ",") {
		val, err := strconv.Atoi(l)
		if err != nil {
			expenses = append(expenses, l)
		} else {
			expenses = append(expenses, val)
		}
	}
	if err := scanner.Err(); err != nil {
		return 0, nil, err
	}
	return departure, expenses, nil
}
