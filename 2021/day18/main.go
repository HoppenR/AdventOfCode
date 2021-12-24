package main

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"log"
	"math"
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

type NodeDepth struct {
	node  *Node
	depth int
}

func main() {
	data, err := ReadFile("input")
	if err != nil {
		log.Fatalln(err)
	}
	fmt.Println("1:", Magnitude(Solve(GenGraphs(data))))
	fmt.Println("2:", LargestAddition(data))
}

func Solve(rootNodes []*Node) *Node {
	result := rootNodes[0]
	for _, node := range rootNodes[1:] {
		result = Reduce(Add(result, node))
	}
	return result
}

func LargestAddition(data [][]byte) int {
	largest := 0
	for i := range data {
		for j := range data {
			if i == j {
				continue
			}
			number := Magnitude(Reduce(Add(DataToGraph(data[i]), DataToGraph(data[j]))))
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
loop:
	if ExplodeNext(root) {
		goto loop
	}
	if SplitNext(root) {
		goto loop
	}
	return root
}

// Explodes the leftmost item with 4 or higher depth
// Returns true if it exploded something
func ExplodeNext(root *Node) bool {
	var todo []NodeDepth
	todo = append(todo, NodeDepth{root, 0})
	for len(todo) > 0 {
		ix := len(todo) - 1
		curNode := todo[ix].node
		curDepth := todo[ix].depth
		for curNode.t == t_node {
			if curDepth >= 4 {
				Explode(curNode)
				return true
			}
			curDepth++
			todo = append(todo, NodeDepth{curNode.right, curDepth})
			curNode = curNode.left
		}
		todo = append(todo[:ix], todo[ix+1:]...)
	}
	return false
}

// Splits the leftmost item with 10 or higher value
// Returns true if it split something
func SplitNext(root *Node) bool {
	var todo []*Node
	todo = append(todo, root)
	for len(todo) > 0 {
		ix := len(todo) - 1
		curNode := todo[ix]
		for curNode != nil {
			if curNode.t == t_value && curNode.value >= 10 {
				Split(curNode)
				return true
			}
			todo = append(todo, curNode.right)
			curNode = curNode.left
		}
		todo = append(todo[:ix], todo[ix+1:]...)
	}
	return false
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
	val := float64(node.value) / 2.0
	node.value = 0
	node.left = &Node{
		t:      t_value,
		value:  int(math.Floor(val)),
		pos:    pos_left,
		parent: node,
	}
	node.right = &Node{
		t:      t_value,
		value:  int(math.Ceil(val)),
		pos:    pos_right,
		parent: node,
	}
}

func LeftAdjacent(node *Node) *Node {
	isLeft := true
	for isLeft {
		isLeft = (node.pos == pos_left)
		if node.parent == nil {
			return nil
		}
		node = node.parent
	}
	node = node.left
	for node.t != t_value {
		node = node.right
	}
	return node
}

func RightAdjacent(node *Node) *Node {
	isRight := true
	for isRight {
		isRight = (node.pos == pos_right)
		if node.parent == nil {
			return nil
		}
		node = node.parent
	}
	node = node.right
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

func GenGraphs(data [][]byte) []*Node {
	var rootNodes []*Node
	for _, line := range data {
		rootNodes = append(rootNodes, DataToGraph(line))
	}
	return rootNodes
}

func DataToGraph(data []byte) *Node {
	buffer := bytes.NewBuffer(data)
	return ParseNode(buffer)
}

func ParseNode(buffer *bytes.Buffer) *Node {
	var node Node
	for {
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
