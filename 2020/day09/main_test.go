package main

import (
	"github.com/stretchr/testify/assert"
	"testing"
)

var exampleData = []int{
	35,
	20,
	15,
	25,
	47,
	40,
	62,
	55,
	65,
	95,
	102,
	117,
	150,
	182,
	127,
	219,
	299,
	277,
	309,
	576,
}

func TestPart1(t *testing.T) {
	target, err := FindNonSum(exampleData, 5)
	assert.Nil(t, err)
	assert.Equal(t, 127, target)
}

func TestPart2(t *testing.T) {
	target, _ := FindNonSum(exampleData, 5)
	ans, err := FindSubset(exampleData, target)
	assert.Nil(t, err)
	assert.Equal(t, 62, ans)
}
