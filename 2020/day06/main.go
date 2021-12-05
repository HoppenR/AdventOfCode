package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"strings"

	"github.com/scylladb/go-set/i32set"
)

func main() {
	forms, err := ReadPassports("input")
	if err != nil {
		log.Fatalln(err)
	}
	fmt.Println("1:", SumCounts(forms, 1))
	fmt.Println("2:", SumCounts(forms, 2))
}

func SumCounts(groups [][]*i32set.Set, ruleset int) (cnt int) {
	for _, g := range groups {
		if ruleset == 1 {
			cnt += i32set.Union(g...).Size()
		} else if ruleset == 2 {
			cnt += i32set.Intersection(g...).Size()
		}
	}
	return
}

func ReadPassports(filename string) ([][]*i32set.Set, error) {
	content, err := ioutil.ReadFile(filename)
	if err != nil {
		return nil, err
	}
	trimmed := strings.TrimRight(string(content), "\n")
	var groups [][]*i32set.Set
	for _, g := range strings.Split(trimmed, "\n\n") {
		var fields []*i32set.Set
		for _, f := range strings.Split(g, "\n") {
			fields = append(fields, i32set.New([]int32(f)...))
		}
		groups = append(groups, fields)
	}
	return groups, nil
}
