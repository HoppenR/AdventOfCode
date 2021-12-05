package main

import (
	"testing"

	"github.com/scylladb/go-set/i32set"
	"github.com/stretchr/testify/assert"
)

var exampleForms = [][]*i32set.Set{
	{i32set.New([]int32("abc")...)},
	{i32set.New('a'), i32set.New('b'), i32set.New('c')},
	{i32set.New([]int32("ab")...), i32set.New([]int32("ac")...)},
	{i32set.New('a'), i32set.New('a'), i32set.New('a'), i32set.New('a')},
	{i32set.New('b')},
}

func TestPart1(t *testing.T) {
	assert.Equal(t, 11, SumCounts(exampleForms, 1))
}

func TestPart2(t *testing.T) {
	assert.Equal(t, 6, SumCounts(exampleForms, 2))
}
