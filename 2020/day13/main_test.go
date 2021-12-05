package main

import (
	"github.com/stretchr/testify/assert"
	"testing"
)

var exampleDepart int = 939

var exampleTimetable = []interface{}{
	7, 13, "x", "x", 59, "x", 31, 19,
}

var exampleTimetable1 = []interface{}{
	17, "x", 13, 19,
}
var exampleTimetable2 = []interface{}{
	67, 7, 59, 61,
}
var exampleTimetable3 = []interface{}{
	67, "x", 7, 59, 61,
}
var exampleTimetable4 = []interface{}{
	67, 7, "x", 59, 61,
}
var exampleTimetable5 = []interface{}{
	1789, 37, 47, 1889,
}

func TestPart1(t *testing.T) {
	assert.Equal(t, 295, EarliestBus(exampleDepart, exampleTimetable))
}

func TestPart2(t *testing.T) {
	assert.Equal(t, 3417, DepartureSeriesTime(exampleTimetable1))
	assert.Equal(t, 754018, DepartureSeriesTime(exampleTimetable2))
	assert.Equal(t, 779210, DepartureSeriesTime(exampleTimetable3))
	assert.Equal(t, 1261476, DepartureSeriesTime(exampleTimetable4))
	assert.Equal(t, 1202161486, DepartureSeriesTime(exampleTimetable5))
}
