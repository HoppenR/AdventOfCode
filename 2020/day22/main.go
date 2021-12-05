package main

import (
	"bytes"
	"container/list"
	"fmt"
	"io/ioutil"
	"log"
	"strconv"
)

func main() {
	decks, err := ReadDecks("input")
	if err != nil {
		log.Fatalln(err)
	}
	fmt.Println("1:", Game(decks))
}

func Game(decks []*list.List) int {
	for {
		winner := Round(decks)
		if decks[1-winner].Len() == 0 {
			return CountScore(decks[winner])
		}
	}
}

func Round(decks []*list.List) int {
	p1card := decks[0].Front()
	p2card := decks[1].Front()
	if value1, value2 := p1card.Value.(int), p2card.Value.(int); value1 > value2 {
		decks[1].Remove(p2card)
		decks[0].MoveToBack(p1card)
		decks[0].PushBack(value2)
		return 0
	} else {
		decks[0].Remove(p1card)
		decks[1].MoveToBack(p2card)
		decks[1].PushBack(value1)
		return 1
	}
}

func CountScore(deck *list.List) (score int) {
	for i, card := 1, deck.Back(); card != nil; i, card = i+1, card.Prev() {
		score += card.Value.(int) * i
	}
	return
}

func ReadDecks(filename string) ([]*list.List, error) {
	content, err := ioutil.ReadFile(filename)
	if err != nil {
		return nil, err
	}
	content = bytes.TrimRight(content, "\n")
	players := bytes.Split(content, []byte("\n\n"))
	var decks []*list.List
	for _, playerstr := range players {
		l := list.New()
		for _, v := range bytes.Split(playerstr, []byte{'\n'})[1:] {
			n, err := strconv.Atoi(string(v))
			if err != nil {
				return nil, err
			}
			l.PushBack(n)
		}
		decks = append(decks, l)
	}
	return decks, nil
}
