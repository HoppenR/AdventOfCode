package main

import (
	"github.com/stretchr/testify/assert"
	"testing"
)

var testSegments = map[string]int{
	"0,0": 1000, "2,0": 1000, "7,0": 1, "8,0": 1000,
	"1,1": 1000, "2,1": 1, "3,1": 1000, "7,1": 1001,
	"2,2": 1001, "4,2": 1000, "6,2": 1000, "7,2": 1, "8,2": 1000,
	"3,3": 1000, "5,3": 2000, "7,3": 1001,
	"1,4": 1, "2,4": 1, "3,4": 2, "4,4": 2001, "5,4": 1, "6,4": 2001, "7,4": 2, "8,4": 1, "9,4": 1,
	"3,5": 1000, "5,5": 2000,
	"2,6": 1000, "6,6": 1000,
	"1,7": 1000, "7,7": 1000,
	"0,8": 1000, "8,8": 1000,
	"0,9": 2, "1,9": 2, "2,9": 2, "3,9": 1, "4,9": 1, "5,9": 1,
}

func TestPart1(t *testing.T) {
	assert.Equal(t, 5, CountOverlap(testSegments, false))
}

func TestPart2(t *testing.T) {
	assert.Equal(t, 12, CountOverlap(testSegments, true))
}
