package main

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"log"
	"os"
)

const (
	t_node int = iota
	t_value
	pos_left
	pos_right
)

type Node struct {
	t      int
	value  int
	pos    int
	right  *Node
	left   *Node
	parent *Node
}

func main() {
	data, err := ReadFile("input")
	if err != nil {
		log.Fatalln(err)
	}
	fmt.Println("1:", Magnitude(Solve(data)))
	fmt.Println("2:", LargestAddition(data))
}

func Solve(data [][]byte) *Node {
	result := DataToGraph(data[0])
	for i := 1; i < len(data); i++ {
		result = Reduce(Add(result, DataToGraph(data[i])))
	}
	return result
}

func LargestAddition(data [][]byte) int {
	var largest int
	for i := range data {
		for j := range data {
			if i == j {
				continue
			}
			number := Magnitude(Solve([][]byte{data[i], data[j]}))
			if number > largest {
				largest = number
			}
		}
	}
	return largest
}

func Magnitude(node *Node) (sum int) {
	if node.t == t_value {
		return node.value
	}
	return 3*Magnitude(node.left) + 2*Magnitude(node.right)
}

func Reduce(root *Node) *Node {
	for ExplodeNext(root, 0) || SplitNext(root) {
		continue
	}
	return root
}

func ExplodeNext(node *Node, depth int) bool {
	if node.t != t_node {
		return false
	}
	if depth >= 4 {
		Explode(node)
		return true
	}
	return ExplodeNext(node.left, depth+1) || ExplodeNext(node.right, depth+1)
}

func SplitNext(node *Node) bool {
	if node.t == t_value {
		if node.value >= 10 {
			Split(node)
			return true
		}
		return false
	}
	return SplitNext(node.left) || SplitNext(node.right)
}

func Explode(node *Node) {
	leftAdj := LeftAdjacent(node)
	if leftAdj != nil {
		leftAdj.value += node.left.value
	}
	rightAjd := RightAdjacent(node)
	if rightAjd != nil {
		rightAjd.value += node.right.value
	}
	node.t = t_value
	node.value = 0
	node.left = nil
	node.right = nil
}

func Split(node *Node) {
	node.t = t_node
	node.left = &Node{
		t:      t_value,
		value:  node.value / 2,
		pos:    pos_left,
		parent: node,
	}
	node.right = &Node{
		t:      t_value,
		value:  (node.value + 1) / 2,
		pos:    pos_right,
		parent: node,
	}
	node.value = 0
}

func LeftAdjacent(node *Node) *Node {
	for {
		if node.parent == nil {
			return nil
		}
		if node.pos != pos_left {
			break
		}
		node = node.parent
	}
	node = node.parent.left
	for node.t != t_value {
		node = node.right
	}
	return node
}

func RightAdjacent(node *Node) *Node {
	for {
		if node.parent == nil {
			return nil
		}
		if node.pos != pos_right {
			break
		}
		node = node.parent
	}
	node = node.parent.right
	for node.t != t_value {
		node = node.left
	}
	return node
}

func Add(lhs, rhs *Node) *Node {
	lhs.pos = pos_left
	rhs.pos = pos_right
	this := &Node{t: t_node, left: lhs, right: rhs}
	lhs.parent = this
	rhs.parent = this
	return this
}

func DataToGraph(datum []byte) *Node {
	buffer := bytes.NewBuffer(datum)
	return ParseNode(buffer)
}

func ParseNode(buffer *bytes.Buffer) *Node {
	var node Node
	nextByte, _ := buffer.ReadByte()
	switch nextByte {
	case '[':
		node.t = t_node
		left := ParseNode(buffer)
		left.parent = &node
		left.pos = pos_left
		node.left = left
		_, _ = buffer.ReadByte() // Consume ','
		right := ParseNode(buffer)
		right.parent = &node
		right.pos = pos_right
		node.right = right
		_, _ = buffer.ReadByte() // Consume ']'
		return &node
	default:
		node.t = t_value
		node.value = int(nextByte - '0')
		return &node
	}
}

func ReadFile(filename string) ([][]byte, error) {
	file, err := os.Open(filename)
	if err != nil {
		return nil, err
	}
	defer file.Close()
	data, err := ioutil.ReadAll(file)
	if err != nil {
		return nil, err
	}
	trimdata := bytes.TrimRight(data, "\n")
	return bytes.Split(trimdata, []byte("\n")), nil
}
