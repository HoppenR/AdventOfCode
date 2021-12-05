package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

type Rule struct {
	options [][]int
	value   string
}

func main() {
	rules, messages, err := ReadRules("input")
	if err != nil {
		log.Fatalln(err)
	}
	fmt.Println("1:", CountMatches(rules, messages))
}

func CountMatches(rules map[int]Rule, messages []string) (cnt int) {
	hashtable := make(map[string]struct{})
	for _, e := range ExpandRule(rules, 0) {
		hashtable[e] = struct{}{}
	}
	for _, msg := range messages {
		if _, ok := hashtable[msg]; ok {
			cnt++
		}
	}
	return
}

func ExpandRule(rules map[int]Rule, ix int) []string {
	if rules[ix].value != "" {
		return []string{rules[ix].value}
	}
	var patterns []string
	for _, opt := range rules[ix].options {
		var newPatterns []string
		for _, d := range opt {
			newPatterns = MergeCombs(newPatterns, ExpandRule(rules, d))
		}
		patterns = append(patterns, newPatterns...)
	}
	return patterns
}

func MergeCombs(one []string, two []string) []string {
	if len(one) == 0 {
		return two
	}
	var ret []string
	for _, os := range one {
		for _, ts := range two {
			ret = append(ret, os+ts)
		}
	}
	return ret
}

func ReadRules(filename string) (map[int]Rule, []string, error) {
	file, err := os.Open(filename)
	if err != nil {
		return nil, nil, err
	}
	defer file.Close()
	// RULES
	scanner := bufio.NewScanner(file)
	rules := make(map[int]Rule, 0)
	for scanner.Scan() {
		line := scanner.Text()
		if line == "" {
			break
		}
		colon := strings.IndexRune(line, ':')
		num, err := strconv.Atoi(line[:colon])
		if err != nil {
			return nil, nil, err
		}
		var rule Rule
		for _, s := range strings.Split(line[colon+2:], "|") {
			var opt []int
			flds := strings.Fields(s)
			for _, f := range flds {
				num, err := strconv.Atoi(f)
				if err != nil {
					rule.value = strings.Trim(f, "\"")
				} else {
					opt = append(opt, num)
				}
			}
			rule.options = append(rule.options, opt)
		}
		rules[num] = rule
	}
	// RECIEVED MESSAGES
	var messages []string
	for scanner.Scan() {
		messages = append(messages, scanner.Text())
	}
	if err := scanner.Err(); err != nil {
		return nil, nil, err
	}
	return rules, messages, nil
}
