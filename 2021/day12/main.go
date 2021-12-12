package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"path/filepath"
	"strings"
	"unicode"
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
	fmt.Println("1:", Traverse(startNode, "", true))
	fmt.Println("2:", Traverse(startNode, "", false))
}

func Traverse(cNode *Node, state string, doubleVisited bool) (count int) {
	if cNode.name == "end" {
		count++
		return
	}
	state = filepath.Join(state, cNode.name)
	for _, nNode := range cNode.connected {
		if nNode.name == "start" {
			continue
		}
		if unicode.IsLower(rune(nNode.name[0])) {
			if Occurrences(state, nNode.name) == 0 {
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

func Occurrences(state, name string) (occ int) {
	visitedNodeNames := strings.Split(state, string(filepath.Separator))
	for _, vnn := range visitedNodeNames {
		if vnn == name {
			occ++
		}
	}
	return
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
