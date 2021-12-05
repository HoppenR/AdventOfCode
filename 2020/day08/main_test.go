package main

import (
	"github.com/stretchr/testify/assert"
	"testing"
)

var examplePrg = []Instr{
	{opcode: nop(+0)},
	{opcode: acc(+1)},
	{opcode: ptr(+4)},
	{opcode: acc(+3)},
	{opcode: ptr(-3)},
	{opcode: acc(-99)},
	{opcode: acc(+1)},
	{opcode: ptr(-4)},
	{opcode: acc(+6)},
}

func TestPart1(t *testing.T) {
	a, _ := TryRun(examplePrg)
	assert.Equal(t, acc(5), a)
}

func TestPart2(t *testing.T) {
	a, err := FindFaultyInstr(examplePrg)
	assert.Nil(t, err)
	assert.Equal(t, acc(8), a)
}
