package main

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

var testArea = &Area{
	min: Pos{20, -10},
	max: Pos{30, -5},
}

func TestPart1(t *testing.T) {
	assert.Equal(t, 45, CalculateFiring(testArea, 1))
}

func TestPart2(t *testing.T) {
	assert.Equal(t, 112, CalculateFiring(testArea, 2))
}
