package main

import (
	"errors"
	"fmt"
	"io/ioutil"
	"log"
	"sort"
	"strings"
)

func main() {
	passes, err := ReadLines("input")
	if err != nil {
		log.Fatalln(err)
	}
	fmt.Println("1:", GetHighestPassID(passes))
	ans2, err := FindMissingID(passes)
	if err != nil {
		log.Fatalln(err)
	}
	fmt.Println("2:", ans2)
}

func GetHighestPassID(passes []string) (highest int) {
	for _, p := range passes {
		if pid := DecodePass(p); pid > highest {
			highest = pid
		}
	}
	return
}

func DecodePass(p string) (pid int) {
	for i, c := range p {
		if strings.IndexRune("BR", c) >= 0 {
			pid += 1 << (len(p) - i - 1)
		}
	}
	return
}

func FindMissingID(passes []string) (int, error) {
	var occupiedIDs []int
	for _, p := range passes {
		occupiedIDs = append(occupiedIDs, DecodePass(p))
	}
	sort.IntSlice.Sort(occupiedIDs)
	for i := 0; i < len(occupiedIDs)-1; i++ {
		if occupiedIDs[i]+2 == occupiedIDs[i+1] {
			return occupiedIDs[i] + 1, nil
		}
	}
	return 0, errors.New("Couldn't find an unoccupied spot between two occupied ones")
}

func ReadLines(filename string) ([]string, error) {
	content, err := ioutil.ReadFile(filename)
	if err != nil {
		return nil, err
	}
	lines := strings.Split(string(content), "\n")
	return lines[:len(lines)-1], nil
}
