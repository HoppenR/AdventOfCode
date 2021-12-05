package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"regexp"
	"strconv"
	"strings"
)

const (
	all = iota
	low
	high
	char
	content
)

func main() {
	policies, err := ReadPolicies("input")
	if err != nil {
		log.Fatalln(err)
	}
	fmt.Println("1:", CountValid(policies, 1))
	fmt.Println("2:", CountValid(policies, 2))
}

func ReadPolicies(filename string) ([][][]byte, error) {
	content, err := ioutil.ReadFile(filename)
	if err != nil {
		return nil, err
	}
	var validr = regexp.MustCompile(`(\d+)-(\d+) (\w): (\w+)`)
	matches := validr.FindAllSubmatch(content, -1)
	return matches, nil
}

func CountValid(matches [][][]byte, ruleset int) (nValid int) {
	if ruleset == 1 {
		for _, m := range matches {
			lo, _ := strconv.Atoi(string(m[low]))
			hi, _ := strconv.Atoi(string(m[high]))
			cnt := strings.Count(string(m[content]), string(m[char]))
			if cnt >= lo && cnt <= hi {
				nValid++
			}
		}
	} else if ruleset == 2 {
		for _, m := range matches {
			lo, _ := strconv.Atoi(string(m[low]))
			hi, _ := strconv.Atoi(string(m[high]))
			if (m[content][lo-1] == m[char][0]) != (m[content][hi-1] == m[char][0]) {
				nValid++
			}
		}
	}
	return
}
