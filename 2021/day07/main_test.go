package main

import (
	"github.com/stretchr/testify/assert"
	"testing"
)

var testPositions = []int{
	16, 1, 2, 0, 4, 2, 7, 1, 2, 14,
}

func TestPart1(t *testing.T) {
	assert.Equal(t, 37, FindBest(testPositions, 16, false))
}

func TestPart2(t *testing.T) {
	assert.Equal(t, 168, FindBest(testPositions, 16, true))
}
