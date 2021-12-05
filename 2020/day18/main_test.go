package main

import (
	"github.com/stretchr/testify/assert"
	"testing"
)

var exampleFile1 = [][]byte{
	[]byte("1+(2*3)+(4*(5+6))"),
}

var exampleFile2 = [][]byte{
	[]byte("2*3+(4*5)"),
}

var exampleFile3 = [][]byte{
	[]byte("5+(8*3+9+3*4*3)"),
}

var exampleFile4 = [][]byte{
	[]byte("5*9*(7*3*3+9*3+(8+6*4))"),
}

var exampleFile5 = [][]byte{
	[]byte("((2+4*9)*(6+9*8+6)+6)+2+4*2"),
}

func TestPart1(t *testing.T) {
	assert.Equal(t, 51, EvalFile(exampleFile1, false))
	assert.Equal(t, 26, EvalFile(exampleFile2, false))
	assert.Equal(t, 437, EvalFile(exampleFile3, false))
	assert.Equal(t, 12240, EvalFile(exampleFile4, false))
	assert.Equal(t, 13632, EvalFile(exampleFile5, false))
}

func TestPart2(t *testing.T) {
	assert.Equal(t, 51, EvalFile(exampleFile1, true))
	assert.Equal(t, 46, EvalFile(exampleFile2, true))
	assert.Equal(t, 1445, EvalFile(exampleFile3, true))
	assert.Equal(t, 669060, EvalFile(exampleFile4, true))
	assert.Equal(t, 23340, EvalFile(exampleFile5, true))
}
