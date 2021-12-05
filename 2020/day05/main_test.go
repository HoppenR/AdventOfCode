package main

import (
	"github.com/stretchr/testify/assert"
	"testing"
)

func TestPart1(t *testing.T) {
	assert.Equal(t, 357, DecodePass("FBFBBFFRLR"))
	examplePasses := []string{"BFFFBBFRRR", "FFFBBBFRRR", "BBFFBBFRLL"}
	assert.Equal(t, 820, GetHighestPassID(examplePasses))
}

func TestPart2(t *testing.T) {
	smolPlanePasses := []string{"FFBRR", "FBFLL", "FBFLR", "FBFRL", "FBFRR",
		"FBBLL", "FBBLR", "FBBRL", "FBBRR", "BFFLL", "BFFLR", "BFFRR", "BFBLL",
		"BFBLR", "BFBRL", "BFBRR", "BBFLL", "BBFLR"}
	ans, err := FindMissingID(smolPlanePasses)
	assert.Nil(t, err)
	assert.Equal(t, 18, ans)
}
