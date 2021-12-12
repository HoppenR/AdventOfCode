package main

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

var (
	start = &Node{}
	A     = &Node{}
	b     = &Node{}
	c     = &Node{}
	d     = &Node{}
	end   = &Node{}
)

func TestSetupNodes(t *testing.T) {
	start.name = "start"
	start.connected = []*Node{A, b}
	A.name = "A"
	A.connected = []*Node{c, b, end, start}
	b.name = "b"
	b.connected = []*Node{d, end, A, start}
	c.name = "c"
	c.connected = []*Node{A}
	d.name = "d"
	d.connected = []*Node{b}
	end.name = "end"
	end.connected = []*Node{A, b}
}

func TestPart1(t *testing.T) {
	assert.Equal(t, 10, Traverse(start, nil, true))
}

func TestPart2(t *testing.T) {
	assert.Equal(t, 36, Traverse(start, nil, false))
}
