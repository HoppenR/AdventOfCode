package main

import (
	"container/list"
	"testing"

	"github.com/stretchr/testify/assert"
)

func SetupExample() *list.List {
	input := list.New()
	input.PushBack(3)
	input.PushBack(8)
	input.PushBack(9)
	input.PushBack(1)
	input.PushBack(2)
	input.PushBack(5)
	input.PushBack(4)
	input.PushBack(6)
	input.PushBack(7)
	return input
}

func TestPart1(t *testing.T) {
	ans, err := PlayCups(SetupExample(), 100)
	assert.Nil(t, err)
	assert.Equal(t, 67384529, ans)
}

func TestPart2(t *testing.T) {
	assert.Equal(t, 0, func() int { return 0 }())
}
