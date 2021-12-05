package main

import (
	"github.com/stretchr/testify/assert"
	"testing"
)

var exampleInstructions = []Instr{
	{'F', 10},
	{'N', 3},
	{'F', 7},
	{'R', 90},
	{'F', 11},
}

func TestPart1(t *testing.T) {
	assert.Equal(t, 25, Traverse(exampleInstructions))
}

func TestPart2(t *testing.T) {
	assert.Equal(t, 286, MoveWaypoint(exampleInstructions))
}
