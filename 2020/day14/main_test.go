package main

import (
	"github.com/stretchr/testify/assert"
	"testing"
)

var exampleProgram1 = []Routine{
	{
		"XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X",
		[]WriteOp{
			{adr: 8, val: 11},
			{adr: 7, val: 101},
			{adr: 8, val: 0},
		},
	},
}

var exampleProgram2 = []Routine{
	{
		"000000000000000000000000000000X1001X",
		[]WriteOp{
			{adr: 42, val: 100},
		},
	},
	{
		"00000000000000000000000000000000X0XX",
		[]WriteOp{
			{adr: 26, val: 1},
		},
	},
}

func TestPart1(t *testing.T) {
	assert.Equal(t, uint64(165), RunProgram(exampleProgram1, 1))
}

func TestPart2(t *testing.T) {
	assert.Equal(t, uint64(208), RunProgram(exampleProgram2, 2))
}
