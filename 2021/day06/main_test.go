package main

import (
	"github.com/stretchr/testify/assert"
	"testing"
)

var testTimers = &[9]int{
	0, 1, 1, 2, 1, 0, 0, 0, 0,
}

func TestPart1(t *testing.T) {
	assert.Equal(t, 5934, Iterate(testTimers, 80))
}

func TestPart2(t *testing.T) {
	assert.Equal(t, 26984457539, Iterate(testTimers, 256-80))
}
