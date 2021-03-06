// Vim: set nofoldenable :
package main

import (
	"github.com/stretchr/testify/assert"
	"testing"
)

var testSubsystem = [][]byte{
	{'[', '(', '{', '(', '<', '(', '(', ')', ')', '[', ']', '>', '[', '[', '{', '[', ']', '{', '<', '(', ')', '<', '>', '>'},
	{'[', '(', '(', ')', '[', '<', '>', ']', ')', ']', '(', '{', '[', '<', '{', '<', '<', '[', ']', '>', '>', '('},
	{'{', '(', '[', '(', '<', '{', '}', '[', '<', '>', '[', ']', '}', '>', '{', '[', ']', '{', '[', '(', '<', '(', ')', '>'},
	{'(', '(', '(', '(', '{', '<', '>', '}', '<', '{', '<', '{', '<', '>', '}', '{', '[', ']', '{', '[', ']', '{', '}'},
	{'[', '[', '<', '[', '(', '[', ']', ')', ')', '<', '(', '[', '[', '{', '}', '[', '[', '(', ')', ']', ']', ']'},
	{'[', '{', '[', '{', '(', '{', '}', ']', '{', '}', '}', '(', '[', '{', '[', '{', '{', '{', '}', '}', '(', '[', ']'},
	{'{', '<', '[', '[', ']', ']', '>', '}', '<', '{', '[', '{', '[', '{', '[', ']', '{', '(', ')', '[', '[', '[', ']'},
	{'[', '<', '(', '<', '(', '<', '(', '<', '{', '}', ')', ')', '>', '<', '(', '[', ']', '(', '[', ']', '(', ')'},
	{'<', '{', '(', '[', '(', '[', '[', '(', '<', '>', '(', ')', ')', '{', '}', ']', '>', '(', '<', '<', '{', '{'},
	{'<', '{', '(', '[', '{', '{', '}', '}', '[', '<', '[', '[', '[', '<', '>', '{', '}', ']', ']', ']', '>', '[', ']', ']'},
}

func TestPart1(t *testing.T) {
	assert.Equal(t, 26397, ErrorScoreSum(testSubsystem))
}

func TestPart2(t *testing.T) {
	assert.Equal(t, 288957, CompletionMiddleScore(testSubsystem))
}
