package main

import (
	"bytes"
	"container/list"
	"fmt"
	"io/ioutil"
	"log"
	"math"
	"strconv"
)

func main() {
	labels, err := ReadLabels("input")
	if err != nil {
		log.Fatalln(err)
	}
	ans1, err := PlayCups(labels, 100)
	if err != nil {
		log.Fatalln(err)
	}
	fmt.Println("1:", ans1)
}

func PlayCups(labels *list.List, rounds int) (int, error) {
	err := PlayRounds(labels, rounds)
	if err != nil {
		return 0, err
	}
	score, err := CupScore(labels)
	if err != nil {
		return 0, err
	}
	return score, nil
}

func PlayRounds(labels *list.List, rounds int) error {
	for i := 0; i < rounds; i++ {
		current := labels.Front()
		RotateL(labels, 1)
		selected := SelectN(labels, 3)
		dest, err := FindDesc(labels, current.Value.(int), selected)
		if err != nil {
			return err
		}
		for i := 0; i < 3; i++ {
			labels.MoveAfter(selected[i], dest)
		}
	}
	return nil
}

func CupScore(labels *list.List) (int, error) {
	label1Elem, err := Find(labels, 1)
	if err != nil {
		return 0, err
	}
	RotateToElement(labels, label1Elem)
	var score int
	for i, e := 0.0, labels.Back(); e != labels.Front(); i, e = i+1.0, e.Prev() {
		score += e.Value.(int) * int(math.Pow(10.0, i))
	}
	return score, nil
}

func FindDesc(l *list.List, n int, taken []*list.Element) (*list.Element, error) {
	for ok := true; ok; ok = FindSlice(taken, n) {
		n = ((n-2)+l.Len())%l.Len() + 1
	}
	destination, err := Find(l, n)
	if err != nil {
		return &list.Element{}, err
	}
	return destination, nil
}

func Find(l *list.List, n int) (*list.Element, error) {
	for i := l.Front(); i != nil; i = i.Next() {
		if i.Value.(int) == n {
			return i, nil
		}
	}
	return &list.Element{}, fmt.Errorf("Could not find element: %d\n", n)
}

func FindSlice(s []*list.Element, n int) bool {
	for _, i := range s {
		if i.Value == n {
			return true
		}
	}
	return false
}

func SelectN(l *list.List, n int) []*list.Element {
	var selected []*list.Element
	item := l.Front()
	for i := 0; i < n; i++ {
		selected = append([]*list.Element{item}, selected...)
		item = item.Next()
	}
	return selected
}

func RotateToElement(l *list.List, e *list.Element) {
	for l.Front() != e {
		RotateL(l, 1)
	}
}

func RotateL(l *list.List, n int) {
	for i := 0; i < n; i++ {
		l.MoveToBack(l.Front())
	}
}

func ReadLabels(filename string) (*list.List, error) {
	content, err := ioutil.ReadFile(filename)
	if err != nil {
		return nil, err
	}
	content = bytes.TrimRight(content, "\n")
	labels := list.New()
	for _, b := range content {
		n, err := strconv.Atoi(string(b))
		if err != nil {
			return nil, err
		}
		labels.PushBack(n)
	}
	return labels, nil
}
