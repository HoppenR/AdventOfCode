package main

import (
	"testing"

	"github.com/scylladb/go-set/strset"
	"github.com/stretchr/testify/assert"
)

var Lines = []*strset.Set{
	strset.New("mxmxvkd", "kfcds", "sqjhc", "nhms"),
	strset.New("trh", "fvjkl", "sbzzf", "mxmxvkd"),
	strset.New("sqjhc", "fvjkl"),
	strset.New("sqjhc", "mxmxvkd", "sbzzf"),
}

var exampleInput = ATI{
	"dairy": []*strset.Set{Lines[0], Lines[1]},
	"fish":  []*strset.Set{Lines[0], Lines[3]},
	"soy":   []*strset.Set{Lines[2]},
}

func TestPart1(t *testing.T) {
	assert.Equal(t, 5, NumSafe(exampleInput, Lines))
}

func TestPart2(t *testing.T) {
	assert.Equal(t, "mxmxvkd,sqjhc,fvjkl", DangerousIngredients(exampleInput))
}
