package main

import (
	"fmt"
	"testing"

	"github.com/stretchr/testify/assert"
)

func GraphStr(node *Node) (ret string) {
	if node.t == t_value {
		return fmt.Sprint(node.value)
	}
	if node.t == t_node {
		ret += "["
		ret += GraphStr(node.left)
		ret += ","
		ret += GraphStr(node.right)
		ret += "]"
	}
	return
}

var testReduce = map[string][]byte{
	"[[[[0,9],2],3],4]":             []byte("[[[[[9,8],1],2],3],4]"),
	"[7,[6,[5,[7,0]]]]":             []byte("[7,[6,[5,[4,[3,2]]]]]"),
	"[[6,[5,[7,0]]],3]":             []byte("[[6,[5,[4,[3,2]]]],1]"),
	"[[3,[2,[8,0]]],[9,[5,[7,0]]]]": []byte("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]"),
}

var testSolve = map[string][][]byte{
	"[[[[0,7],4],[[7,8],[6,0]]],[8,1]]": {
		[]byte("[[[[4,3],4],4],[7,[[8,4],9]]]"), []byte("[1,1]"),
	},
	"[[[[5,0],[7,4]],[5,5]],[6,6]]": {
		[]byte("[1,1]"),
		[]byte("[2,2]"),
		[]byte("[3,3]"),
		[]byte("[4,4]"),
		[]byte("[5,5]"),
		[]byte("[6,6]"),
	},
	"[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]": {
		[]byte("[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]"),
		[]byte("[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]"),
		[]byte("[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]"),
		[]byte("[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]"),
		[]byte("[7,[5,[[3,8],[1,4]]]]"),
		[]byte("[[2,[2,2]],[8,[8,1]]]"),
		[]byte("[2,9]"),
		[]byte("[1,[[[9,3],9],[[9,0],[0,7]]]]"),
		[]byte("[[[5,[7,4]],7],1]"),
		[]byte("[[[[4,2],2],6],[8,7]]"),
	},
	"[[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]": {
		[]byte("[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]"),
		[]byte("[[[5,[2,8]],4],[5,[[9,9],0]]]"),
		[]byte("[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]"),
		[]byte("[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]"),
		[]byte("[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]"),
		[]byte("[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]"),
		[]byte("[[[[5,4],[7,7]],8],[[8,3],8]]"),
		[]byte("[[9,3],[[9,9],[6,[4,9]]]]"),
		[]byte("[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]"),
		[]byte("[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"),
	},
}

var testMagnitude = map[int][]byte{
	143:  []byte("[[1,2],[[3,4],5]]"),
	1384: []byte("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"),
	445:  []byte("[[[[1,1],[2,2]],[3,3]],[4,4]]"),
	791:  []byte("[[[[3,0],[5,3]],[4,4]],[5,5]]"),
	1137: []byte("[[[[5,0],[7,4]],[5,5]],[6,6]]"),
	3488: []byte("[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"),
	4140: []byte("[[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]"),
}

func TestPart1(t *testing.T) {
	for ans, data := range testReduce {
		assert.Equal(t, ans, GraphStr(Reduce(DataToGraph(data))))
	}
	for ans, data := range testSolve {
		assert.Equal(t, ans, GraphStr(Solve(GenGraphs(data))))
	}
	for ans, data := range testMagnitude {
		assert.Equal(t, ans, Magnitude(DataToGraph(data)))
	}
}

var testLargestAddition = map[int][][]byte{
	3993: {
		[]byte("[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]"),
		[]byte("[[[5,[2,8]],4],[5,[[9,9],0]]]"),
		[]byte("[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]"),
		[]byte("[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]"),
		[]byte("[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]"),
		[]byte("[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]"),
		[]byte("[[[[5,4],[7,7]],8],[[8,3],8]]"),
		[]byte("[[9,3],[[9,9],[6,[4,9]]]]"),
		[]byte("[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]"),
		[]byte("[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"),
	},
}

func TestPart2(t *testing.T) {
	for ans, data := range testLargestAddition {
		assert.Equal(t, ans, LargestAddition(data))
	}
}
