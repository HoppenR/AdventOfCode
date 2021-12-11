package main

import (
	"math"
	"testing"

	"github.com/stretchr/testify/assert"
)

var testGrid = map[Point]*Octopus{
	{0, 0}: {5, false}, {1, 0}: {4, false}, {2, 0}: {8, false}, {3, 0}: {3, false}, {4, 0}: {1, false},
	{5, 0}: {4, false}, {6, 0}: {3, false}, {7, 0}: {2, false}, {8, 0}: {2, false}, {9, 0}: {3, false},
	{0, 1}: {2, false}, {1, 1}: {7, false}, {2, 1}: {4, false}, {3, 1}: {5, false}, {4, 1}: {8, false},
	{5, 1}: {5, false}, {6, 1}: {4, false}, {7, 1}: {7, false}, {8, 1}: {1, false}, {9, 1}: {1, false},
	{0, 2}: {5, false}, {1, 2}: {2, false}, {2, 2}: {6, false}, {3, 2}: {4, false}, {4, 2}: {5, false},
	{5, 2}: {5, false}, {6, 2}: {6, false}, {7, 2}: {1, false}, {8, 2}: {7, false}, {9, 2}: {3, false},
	{0, 3}: {6, false}, {1, 3}: {1, false}, {2, 3}: {4, false}, {3, 3}: {1, false}, {4, 3}: {3, false},
	{5, 3}: {3, false}, {6, 3}: {6, false}, {7, 3}: {1, false}, {8, 3}: {4, false}, {9, 3}: {6, false},
	{0, 4}: {6, false}, {1, 4}: {3, false}, {2, 4}: {5, false}, {3, 4}: {7, false}, {4, 4}: {3, false},
	{5, 4}: {8, false}, {6, 4}: {5, false}, {7, 4}: {4, false}, {8, 4}: {7, false}, {9, 4}: {8, false},
	{0, 5}: {4, false}, {1, 5}: {1, false}, {2, 5}: {6, false}, {3, 5}: {7, false}, {4, 5}: {5, false},
	{5, 5}: {2, false}, {6, 5}: {4, false}, {7, 5}: {6, false}, {8, 5}: {4, false}, {9, 5}: {5, false},
	{0, 6}: {2, false}, {1, 6}: {1, false}, {2, 6}: {7, false}, {3, 6}: {6, false}, {4, 6}: {8, false},
	{5, 6}: {4, false}, {6, 6}: {1, false}, {7, 6}: {7, false}, {8, 6}: {2, false}, {9, 6}: {1, false},
	{0, 7}: {6, false}, {1, 7}: {8, false}, {2, 7}: {8, false}, {3, 7}: {2, false}, {4, 7}: {8, false},
	{5, 7}: {8, false}, {6, 7}: {1, false}, {7, 7}: {1, false}, {8, 7}: {3, false}, {9, 7}: {4, false},
	{0, 8}: {4, false}, {1, 8}: {8, false}, {2, 8}: {4, false}, {3, 8}: {6, false}, {4, 8}: {8, false},
	{5, 8}: {4, false}, {6, 8}: {8, false}, {7, 8}: {5, false}, {8, 8}: {5, false}, {9, 8}: {4, false},
	{0, 9}: {5, false}, {1, 9}: {2, false}, {2, 9}: {8, false}, {3, 9}: {3, false}, {4, 9}: {7, false},
	{5, 9}: {5, false}, {6, 9}: {1, false}, {7, 9}: {5, false}, {8, 9}: {2, false}, {9, 9}: {6, false},
}

func TestPart1(t *testing.T) {
	assert.Equal(t, 1656, SimulateGrid(testGrid, 100, false))
}

func TestPart2(t *testing.T) {
	assert.Equal(t, 195, SimulateGrid(testGrid, math.MaxInt, true))
}
