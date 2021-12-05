package main

import (
	"github.com/stretchr/testify/assert"
	"testing"
)

var exampleR = Rulebook{
	"light red":    {{"bright white", 2}, {"muted yellow", 2}},
	"dark orange":  {{"bright white", 3}, {"muted yellow", 4}},
	"bright white": {{"shiny gold", 1}},
	"muted yellow": {{"shiny gold", 2}, {"faded blue", 9}},
	"shiny gold":   {{"dark olive", 1}, {"vibrant plum", 2}},
	"dark olive":   {{"faded blue", 3}, {"dotted black", 4}},
	"vibrant plum": {{"faded blue", 5}, {"dotted black", 6}},
	"faded blue":   {},
	"dotted black": {},
}

var exampleR2 = Rulebook{
	"shiny gold":  {{"dark red", 2}},
	"dark red":    {{"dark orange", 2}},
	"dark orange": {{"dark yellow", 2}},
	"dark yellow": {{"dark green", 2}},
	"dark green":  {{"dark blue", 2}},
	"dark blue":   {{"dark violet", 2}},
	"dark violet": {},
}

func TestPart1(t *testing.T) {
	assert.Equal(t, 4, exampleR.CountGoldContainers())
}

func TestPart2(t *testing.T) {
	assert.Equal(t, 32, exampleR.BagsContained(exampleR["shiny gold"]))
	assert.Equal(t, 126, exampleR2.BagsContained(exampleR2["shiny gold"]))
}
