package main

import (
	"github.com/stretchr/testify/assert"
	"testing"
)

/*
forward 5
down 5
forward 8
up 3
down 8
forward 2
*/

var testDeltas = []Pos{
	{X: 5, Y: 0},
	{X: 0, Y: 5},
	{X: 8, Y: 0},
	{X: 0, Y: -3},
	{X: 0, Y: 8},
	{X: 2, Y: 0},
}

func TestPart1(t *testing.T) {
	assert.Equal(t, 150, SimPos(testDeltas, false))
}

func TestPart2(t *testing.T) {
	assert.Equal(t, 900, SimPos(testDeltas, true))
}
