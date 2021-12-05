package main

import (
	"github.com/stretchr/testify/assert"
	"testing"
)

func TestPart1(t *testing.T) {
	ans, err := FindEntries([]int{1721, 979, 366, 299, 675, 1456}, 2, 2020)
	assert.Nil(t, err)
	assert.Equal(t, 514579, ans)
}

func TestPart2(t *testing.T) {
	ans, err := FindEntries([]int{1721, 979, 366, 299, 675, 1456}, 3, 2020)
	assert.Nil(t, err)
	assert.Equal(t, 241861950, ans)
}
