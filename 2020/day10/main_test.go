package main

import (
	"github.com/stretchr/testify/assert"
	"testing"
)

var exampleSmall = []int{
	0, // Outlet
	1,
	4,
	5,
	6,
	7,
	10,
	11,
	12,
	15,
	16,
	19,
	22, // Device
}

var exampleBig = []int{
	0, // Outlet
	1,
	2,
	3,
	4,
	7,
	8,
	9,
	10,
	11,
	14,
	17,
	18,
	19,
	20,
	23,
	24,
	25,
	28,
	31,
	32,
	33,
	34,
	35,
	38,
	39,
	42,
	45,
	46,
	47,
	48,
	49,
	52, // Device
}

func TestPart1(t *testing.T) {
	assert.Equal(t, 35, JoltGapValue(exampleSmall))
	assert.Equal(t, 220, JoltGapValue(exampleBig))
}

func TestPart2(t *testing.T) {
	assert.Equal(t, 8, Combinations(exampleSmall))
	assert.Equal(t, 19208, Combinations(exampleBig))
}
