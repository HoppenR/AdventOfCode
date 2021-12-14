package main

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

var testTemplate = map[string]int{
	"NN": 1,
	"NC": 1,
	"CB": 1,
}

var testPairs = map[string]string{
	"CH": "B",
	"HH": "N",
	"CB": "H",
	"NH": "C",
	"HB": "C",
	"HC": "B",
	"HN": "C",
	"NN": "C",
	"BH": "H",
	"NC": "B",
	"NB": "B",
	"BN": "B",
	"BB": "N",
	"BC": "B",
	"CC": "N",
	"CN": "C",
}

func TestPart1(t *testing.T) {
	assert.Equal(t, 1588, StepN(testTemplate, testPairs, 10))
}

func TestPart2(t *testing.T) {
	assert.Equal(t, 2188189693529, StepN(testTemplate, testPairs, 40))
}
