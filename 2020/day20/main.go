package main

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"log"
	"strconv"
)

type Tile struct {
	content [][]byte
	ID      int
}

type SideScore struct {
	score int
	ID    int
}

func main() {
	tiles, err := ReadLines("input")
	if err != nil {
		log.Fatalln(err)
	}
	fmt.Println("1:", UniqueIDScore(tiles))
}

func UniqueIDScore(tiles []Tile) int {
	sides := make([]SideScore, 0)
	for i := range tiles {
		for _, v := range GetEdges(tiles, i) {
			sides = append(sides, SideScore{SideToInt(v), tiles[i].ID})
		}
	}
	return FindUnique(sides)
}

func FindUnique(sides []SideScore) int {
	// Filter out all sides with occurrence > 1
	occurrences := make(map[int]int)
	for _, v := range sides {
		occurrences[v.score]++
	}
	targets := make([]int, 0)
	for k, v := range occurrences {
		if v != 1 {
			targets = append(targets, k)
		}
	}
	// find which sides own those scores and count unique sides of every ID
	uniquesOwned := make(map[int]int, 0)
	for _, t := range targets {
		for _, s := range sides {
			if s.score == t {
				uniquesOwned[s.ID]++
			}
		}
	}
	// For a corner piece it should have 4 unique sides
	// (Non connecting side)
	// (Non connecting top/bottom)
	// (Non connecting FLIPPED side)
	// (Non connecting MIRRORED top/bottom)
	sum := 1
	for k, v := range uniquesOwned {
		if v == 4 {
			sum *= k
		}
	}
	return sum
}

func GetEdges(tiles []Tile, ix int) [][]byte {
	ret := make([][]byte, 0)
	// Top
	ret = append(ret, tiles[ix].content[0])
	// Bot
	ret = append(ret, tiles[ix].content[len(tiles[ix].content)-1])
	// Left, Right
	var left, right []byte
	// Flipped Left, Right
	var fLeft, fRight []byte
	for i := range tiles[ix].content {
		c := tiles[ix].content[i]
		oC := tiles[ix].content[len(c)-i-1]
		left = append(left, c[0])
		right = append(right, c[len(c)-1])
		fLeft = append(fLeft, oC[0])
		fRight = append(fRight, oC[len(c)-1])
	}
	ret = append(ret, left)
	ret = append(ret, right)
	ret = append(ret, fLeft)
	ret = append(ret, fRight)
	// Mirrored Top, Bottom
	ret = append(ret, Mirror(tiles[ix].content[0]))
	ret = append(ret, Mirror(tiles[ix].content[len(tiles[ix].content)-1]))
	return ret
}

func SideToInt(content []byte) int {
	// Interprets the side as a binary number
	var ret int
	for i, v := range content {
		if v == '#' {
			ret += (1 << i)
		}
	}
	return ret
}

func Mirror(content []byte) []byte {
	ret := make([]byte, 0)
	for i := range content {
		ret = append(ret, content[len(content)-1-i])
	}
	return ret
}

func ReadLines(filename string) ([]Tile, error) {
	content, err := ioutil.ReadFile(filename)
	if err != nil {
		return nil, err
	}
	content = bytes.TrimRight(content, "\n")
	picstr := bytes.Split(content, []byte("\n\n"))
	tiles := make([]Tile, 0)
	for _, ps := range picstr {
		var t Tile
		lines := bytes.Split(ps, []byte{'\n'})
		t.ID, err = strconv.Atoi(string(lines[0][5:9]))
		if err != nil {
			return nil, err
		}
		t.content = lines[1:]
		tiles = append(tiles, t)
	}
	return tiles, nil
}
