package main

import (
	"github.com/stretchr/testify/assert"
	"testing"
)

var testDepths = []int{
	199,
	200,
	208,
	210,
	200,
	207,
	240,
	269,
	260,
	263,
}

func TestPart1(t *testing.T) {
	assert.Equal(t, 7, FindIncrementsWindow(testDepths, 1))
}

func TestPart2(t *testing.T) {
	assert.Equal(t, 5, FindIncrementsWindow(testDepths, 3))
}
