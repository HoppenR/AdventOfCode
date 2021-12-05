package main

import (
	"container/list"
	"testing"

	"github.com/stretchr/testify/assert"
)

func SetupExample() []*list.List {
	p1l := list.New()
	p2l := list.New()
	// P1
	p1l.PushBack(9)
	p1l.PushBack(2)
	p1l.PushBack(6)
	p1l.PushBack(3)
	p1l.PushBack(1)
	// P2
	p2l.PushBack(5)
	p2l.PushBack(8)
	p2l.PushBack(4)
	p2l.PushBack(7)
	p2l.PushBack(10)
	return []*list.List{p1l, p2l}
}

func TestPart1(t *testing.T) {
	exampleDecks := SetupExample()
	assert.Equal(t, 306, Game(exampleDecks))
}

func TestPart2(t *testing.T) {
	assert.Equal(t, 0, func() int { return 0 }())
}
