package main

import (
	"github.com/stretchr/testify/assert"
	"regexp"
	"testing"
)

var Policies = regexp.MustCompile(`(\d+)-(\d+) (\w): (\w+)`).FindAllSubmatch(
	[]byte("1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc\n"), -1)

func TestPart1(t *testing.T) {
	assert.Equal(t, 2, CountValid(Policies, 1))
}

func TestPart2(t *testing.T) {
	assert.Equal(t, 1, CountValid(Policies, 2))
}
