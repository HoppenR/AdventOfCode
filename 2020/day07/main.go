package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"strconv"
	"strings"
)

type Condition struct {
	name string
	qty  int
}
type Rule []Condition
type Rulebook map[string]Rule

func main() {
	rulebook, err := ReadRulebook("input")
	if err != nil {
		log.Fatalln(err)
	}
	fmt.Println("1:", rulebook.CountGoldContainers())
	fmt.Println("2:", rulebook.BagsContained(rulebook["shiny gold"]))
}

func (rulebook Rulebook) CountGoldContainers() (count int) {
	for _, r := range rulebook {
		if rulebook.FindShinyGold(r) {
			count++
		}
	}
	return
}

func (rulebook Rulebook) FindShinyGold(r Rule) bool {
	for _, c := range r {
		if c.name == "shiny gold" || rulebook.FindShinyGold(rulebook[c.name]) {
			return true
		}
	}
	return false
}

func (rulebook Rulebook) BagsContained(r Rule) (count int) {
	for _, c := range r {
		count += c.qty * (rulebook.BagsContained(rulebook[c.name]) + 1)
	}
	return
}

func ReadRulebook(filename string) (Rulebook, error) {
	content, err := ioutil.ReadFile(filename)
	if err != nil {
		return nil, err
	}
	trimmed := strings.TrimRight(string(content), "\n")
	rulebook := make(Rulebook, 0)
	for _, line := range strings.Split(trimmed, "\n") {
		var r Rule
		linewords := strings.Fields(line)
		name := strings.Join(linewords[:2], " ")
		for _, v := range strings.Split(strings.Join(linewords[4:], " "), ",") {
			if v == "no other bags." {
				break
			}
			condwords := strings.Fields(v)
			qty, err := strconv.Atoi(condwords[0])
			if err != nil {
				return nil, err
			}
			name := strings.Join(condwords[1:3], " ")
			r = append(r, Condition{name, qty})
		}
		rulebook[name] = r
	}
	return rulebook, nil
}
