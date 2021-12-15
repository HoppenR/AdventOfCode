package main

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"log"
	"math"

	"container/heap"
)

type Point struct {
	x, y int
}

type Element struct {
	point Point
	cost  int
}

type QueueHeap []Element

var compOffsets = []Point{
	{1, 0}, {-1, 0}, {0, 1}, {0, -1},
}

func main() {
	costs, err := LoadGrid("input")
	if err != nil {
		log.Fatalln(err)
	}
	fmt.Println("1:", ShortestPath(costs))
	fmt.Println("2:", ShortestPath(ScaleMap(costs, 5)))
}

func ShortestPath(costs map[Point]int) int {
	unvisited := make(map[Point]struct{})
	for k := range costs {
		unvisited[k] = struct{}{}
	}
	queue := new(QueueHeap)
	heap.Push(queue, Element{Point{0, 0}, 0})
	sideLen := int(math.Sqrt(float64(len(costs))))
	for queue.Len() > 0 {
		element := heap.Pop(queue).(Element)
		if _, ok := unvisited[element.point]; !ok {
			continue
		}
		if element.point.x == sideLen-1 && element.point.y == sideLen-1 {
			return element.cost
		}
		for _, Δc := range compOffsets {
			compPoint := Point{element.point.x + Δc.x, element.point.y + Δc.y}
			if _, ok := costs[compPoint]; !ok {
				continue
			}
			if _, ok := unvisited[compPoint]; !ok {
				continue
			}
			compCost := element.cost + costs[compPoint]
			heap.Push(queue, Element{compPoint, compCost})
		}
		delete(unvisited, element.point)
	}
	panic("Unreachable")
}

func ScaleMap(costs map[Point]int, scale int) map[Point]int {
	newCosts := make(map[Point]int)
	sideLen := int(math.Sqrt(float64(len(costs))))
	for y := 0; y < scale; y += 1 {
		for x := 0; x < scale; x += 1 {
			for k, v := range costs {
				newCosts[Point{k.x + x*sideLen, k.y + y*sideLen}] = (v+x+y)%10 + (v+x+y)/10
			}
		}
	}
	return newCosts
}

func (spt QueueHeap) Len() int {
	return len(spt)
}

func (spt QueueHeap) Less(i, j int) bool {
	return spt[i].cost < spt[j].cost
}

func (spt QueueHeap) Swap(i, j int) {
	spt[i], spt[j] = spt[j], spt[i]
}

func (spt *QueueHeap) Push(e interface{}) {
	*spt = append(*spt, e.(Element))
}
func (spt *QueueHeap) Pop() (e interface{}) {
	*spt, e = (*spt)[:len(*spt)-1], (*spt)[len(*spt)-1]
	return
}

func LoadGrid(filename string) (map[Point]int, error) {
	content, err := ioutil.ReadFile(filename)
	if err != nil {
		return nil, err
	}
	costs := make(map[Point]int)
	for y, row := range bytes.Split(content, []byte("\n")) {
		for x, col := range row {
			costs[Point{x, y}] = int(col - '0')
		}
	}
	return costs, nil
}
