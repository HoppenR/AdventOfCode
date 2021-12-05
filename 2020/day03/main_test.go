package main

import (
	"github.com/stretchr/testify/assert"
	"testing"
)

var exampleMap = []string{
	"..##.......",
	"#...#...#..",
	".#....#..#.",
	"..#.#...#.#",
	".#...##..#.",
	"..#.##.....",
	".#.#.#....#",
	".#........#",
	"#.##...#...",
	"#...##....#",
	".#..#...#.#",
}

func TestPart1(t *testing.T) {
	assert.Equal(t, 7, CountTrees(exampleMap, Slope{3, 1}))
}

func TestPart2(t *testing.T) {
	assert.Equal(t, 336, MultiplySlopes(exampleMap, SlopeTracks))
}
