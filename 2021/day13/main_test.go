package main

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

var testPoints = map[Point]struct{}{
	{6, 10}:  {},
	{0, 14}:  {},
	{9, 10}:  {},
	{0, 3}:   {},
	{10, 4}:  {},
	{4, 11}:  {},
	{6, 0}:   {},
	{6, 12}:  {},
	{4, 1}:   {},
	{0, 13}:  {},
	{10, 12}: {},
	{3, 4}:   {},
	{3, 0}:   {},
	{8, 4}:   {},
	{1, 10}:  {},
	{2, 14}:  {},
	{8, 10}:  {},
	{9, 0}:   {},
}

var testFolds = []interface{}{
	yFold(7),
	xFold(5),
}

func TestPart1(t *testing.T) {
	assert.Equal(t, "17", Fold(testPoints, testFolds, 1))
}

func TestPart2(t *testing.T) {
	assert.Equal(
		t,
		"\n#####\n#   #\n#   #\n#   #\n#####",
		Fold(testPoints, testFolds, -1),
	)
}
