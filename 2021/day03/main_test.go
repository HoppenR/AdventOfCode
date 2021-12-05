package main

import (
	"github.com/stretchr/testify/assert"
	"testing"
)

var testDiagnostic = []int64{
	0b00100,
	0b11110,
	0b10110,
	0b10111,
	0b10101,
	0b01111,
	0b00111,
	0b11100,
	0b10000,
	0b11001,
	0b00010,
	0b01010,
}

func TestPart1(t *testing.T) {
	assert.Equal(t, int64(198), PowerConsumption(testDiagnostic, 5))
}

func TestPart2(t *testing.T) {
	assert.Equal(t, int64(230), LifeSupport(testDiagnostic, 5))
}
