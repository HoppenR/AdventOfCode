package main

import (
	"github.com/stretchr/testify/assert"
	"testing"
)

var exampleState = map[Cube]struct{}{
	{1, 0, 0, 0}: {},
	{2, 1, 0, 0}: {},
	{0, 2, 0, 0}: {},
	{1, 2, 0, 0}: {},
	{2, 2, 0, 0}: {},
}

func TestPart1(t *testing.T) {
	assert.Equal(t, 112, BootSource(exampleState, 3))
}

func TestPart2(t *testing.T) {
	assert.Equal(t, 848, BootSource(exampleState, 4))
}
