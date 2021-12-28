package main

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

var testStartPos = &PlayerStats{
	p1: 4,
	p2: 8,
}

func TestPart1(t *testing.T) {
	assert.Equal(t, 739785, RollDeterministic(testStartPos))
}

func TestPart2(t *testing.T) {
	assert.Equal(t, 444356092776315, RollQuantum(testStartPos))
}
