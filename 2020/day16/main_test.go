package main

import (
	"github.com/stretchr/testify/assert"
	"testing"
)

var exFields1 = []Field{
	{lows: []int{1, 5}, highs: []int{3, 7}, name: "class"},
	{lows: []int{6, 33}, highs: []int{11, 44}, name: "row"},
	{lows: []int{13, 45}, highs: []int{40, 50}, name: "seat"},
}

var exTickets1 = []Ticket{
	{values: []int{7, 3, 47}},
	{values: []int{40, 4, 50}},
	{values: []int{55, 2, 20}},
	{values: []int{38, 6, 12}},
}

var exFields2 = []Field{
	{lows: []int{0, 4}, highs: []int{1, 19}, name: "departure class"},
	{lows: []int{0, 8}, highs: []int{5, 19}, name: "departure row"},
	{lows: []int{0, 16}, highs: []int{13, 19}, name: "seat"},
}

var exMyTicket = Ticket{
	values: []int{11, 12, 13},
}

var exTickets2 = []Ticket{
	{values: []int{3, 9, 18}},
	{values: []int{15, 1, 5}},
	{values: []int{5, 14, 9}},
}

func TestPart1(t *testing.T) {
	_, ans := ValidateTickets(exFields1, exTickets1)
	assert.Equal(t, 71, ans)
}

func TestPart2(t *testing.T) {
	assert.Equal(t, 132, DetermineFields(exFields2, exTickets2, exMyTicket))
	exFields2[1].name = "row"
	exFields2[2].name = "departure seat"
	assert.Equal(t, 156, DetermineFields(exFields2, exTickets2, exMyTicket))
	exFields2[1].name = "departure row"
	exFields2[2].name = "seat"
}
