package main

import (
	"github.com/stretchr/testify/assert"
	"testing"
)

var exampleInputs = [][]int{
	{2, 3, 1},
	{3, 2, 1},
}

func TestPart1(t *testing.T) {
	assert.Equal(t, 78, MemoryGame(exampleInputs[0], 2020))
	assert.Equal(t, 438, MemoryGame(exampleInputs[1], 2020))
}

func TestPart2(t *testing.T) {
	assert.Equal(t, 6895259, MemoryGame(exampleInputs[0], 30000000))
	assert.Equal(t, 18, MemoryGame(exampleInputs[1], 30000000))
}
