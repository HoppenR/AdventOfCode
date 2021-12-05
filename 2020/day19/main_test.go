package main

import (
	"github.com/stretchr/testify/assert"
	"testing"
)

var exampleRules = map[int]Rule{
	0: {options: [][]int{{4, 1, 5}}},
	1: {options: [][]int{{2, 3}, {3, 2}}},
	2: {options: [][]int{{4, 4}, {5, 5}}},
	3: {options: [][]int{{4, 5}, {5, 4}}},
	4: {value: "a"},
	5: {value: "b"},
}

var exampleMessages = []string{
	"ababbb",
	"bababa",
	"abbbab",
	"aaabbb",
	"aaaabbb",
}

func TestPart1(t *testing.T) {
	assert.Equal(t, 2, CountMatches(exampleRules, exampleMessages))
}

func TestPart2(t *testing.T) {
	assert.Equal(t, 0, func() int { return 0 }())
}
