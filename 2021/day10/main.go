package main

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"log"
	"sort"
)

type Stack struct {
	items []byte
}

var ByteScore = map[byte]int{
	// Illegal:
	')': 3,
	']': 57,
	'}': 1197,
	'>': 25137,
	// Completion:
	'(': 1,
	'[': 2,
	'{': 3,
	'<': 4,
}

func main() {
	subsystem, err := ReadSubsystem("input")
	if err != nil {
		log.Fatalln(err)
	}
	fmt.Println("1:", ErrorScoreSum(subsystem))
	fmt.Println("2:", CompletionMiddleScore(subsystem))
}

func ErrorScoreSum(subsystem [][]byte) (score int) {
	for _, line := range subsystem {
		score += ErrorScore(line)
	}
	return
}

func ErrorScore(line []byte) int {
	var stack Stack
	for _, b := range line {
		switch b {
		case '(', '[', '{', '<':
			stack.Push(b)
		case ')', ']', '}', '>':
			match := stack.Pop()
			if !(match == b-1 || match == b-2) {
				return ByteScore[b]
			}
		}
	}
	return 0
}

func CompletionMiddleScore(subsystem [][]byte) int {
	var scores []int
	for _, line := range subsystem {
		score := CompletionScore(line)
		if score != 0 {
			scores = append(scores, score)
		}
	}
	sort.Sort(sort.IntSlice(scores))
	return scores[len(scores)/2]
}

func CompletionScore(line []byte) (score int) {
	var stack Stack
	for _, b := range line {
		switch b {
		case '(', '[', '{', '<':
			stack.Push(b)
		case ')', ']', '}', '>':
			match := stack.Pop()
			if !(match == b-1 || match == b-2) {
				return 0
			}
		}
	}
	for len(stack.items) > 0 {
		score *= 5
		score += ByteScore[stack.Pop()]
	}
	return
}

func (stack *Stack) Push(b byte) {
	stack.items = append(stack.items, b)
}

func (stack *Stack) Pop() byte {
	b := stack.items[len(stack.items)-1]
	stack.items = stack.items[:len(stack.items)-1]
	return b
}

func ReadSubsystem(filename string) ([][]byte, error) {
	content, err := ioutil.ReadFile(filename)
	if err != nil {
		return nil, err
	}
	return bytes.Split(content, []byte("\n")), nil
}
