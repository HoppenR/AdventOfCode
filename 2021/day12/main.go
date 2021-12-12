package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strings"
)

type Node struct {
	name      string
	connected []*Node
}

func main() {
	startNode, err := ParseNodes("input")
	if err != nil {
		log.Fatalln(err)
	}
	fmt.Println("1:", Traverse(startNode, nil, true))
	fmt.Println("2:", Traverse(startNode, nil, false))
}

func Traverse(cNode *Node, state []string, doubleVisited bool) (count int) {
	if cNode.name == "end" {
		count++
		return
	}
	state = append(state, cNode.name)
	for _, nNode := range cNode.connected {
		if nNode.name == "start" {
			continue
		}
		if nNode.name[0] >= 'a' {
			if !Exists(state, nNode.name) {
				count += Traverse(nNode, state, doubleVisited)
			} else if !doubleVisited {
				count += Traverse(nNode, state, true)
			}
		} else {
			count += Traverse(nNode, state, doubleVisited)
		}
	}
	return
}

func Exists(state []string, name string) bool {
	for _, vnn := range state {
		if vnn == name {
			return true
		}
	}
	return false
}

func ParseNodes(filename string) (*Node, error) {
	file, err := os.Open(filename)
	if err != nil {
		return nil, err
	}
	defer file.Close()
	scanner := bufio.NewScanner(file)
	nodes := make(map[string]*Node)
	for scanner.Scan() {
		flds := strings.Split(scanner.Text(), "-")
		curName, nextName := flds[0], flds[1]
		var cNode, nNode *Node
		if current, ok := nodes[curName]; ok {
			cNode = current
		} else {
			cNode = &Node{name: curName}
			nodes[curName] = cNode
		}
		if next, ok := nodes[nextName]; ok {
			nNode = next
		} else {
			nNode = &Node{name: nextName}
			nodes[nextName] = nNode
		}
		cNode.connected = append(cNode.connected, nNode)
		nNode.connected = append(nNode.connected, cNode)
	}
	if err := scanner.Err(); err != nil {
		return nil, err
	}
	return nodes["start"], nil
}
