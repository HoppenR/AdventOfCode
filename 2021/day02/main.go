package main

import (
	"bufio"
	"errors"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

type Pos struct {
	X   int
	Y   int
	aim int
}

func main() {
	deltaPos, err := ReadDeltas("input")
	if err != nil {
		log.Fatalln(err)
	}
	fmt.Println("1:", SimPos(deltaPos, false))
	fmt.Println("2:", SimPos(deltaPos, true))
}

func (lh *Pos) add(rh Pos) {
	lh.X += rh.X
	lh.Y += rh.Y
}

func (lh *Pos) addAim(rh Pos) {
	lh.X += rh.X
	lh.Y += rh.X * lh.aim
	lh.aim += rh.Y
}

func SimPos(deltas []Pos, useAim bool) int {
	var curPos Pos
	for _, v := range deltas {
		if !useAim {
			curPos.add(v)
		} else {
			curPos.addAim(v)
		}
	}
	return curPos.X * curPos.Y
}

func ReadDeltas(filename string) ([]Pos, error) {
	file, err := os.Open(filename)
	if err != nil {
		return nil, err
	}
	defer file.Close()
	scanner := bufio.NewScanner(file)
	deltaPos := make([]Pos, 0)
	for scanner.Scan() {
		var pos Pos
		fields := strings.Fields(scanner.Text())
		val, err := strconv.Atoi(fields[1])
		if err != nil {
			return nil, err
		}
		switch fields[0] {
		case "forward":
			pos.X = val
		case "up":
			pos.Y = -val
		case "down":
			pos.Y = val
		default:
			return nil, errors.New("unhandled movement:" + fields[0])
		}
		deltaPos = append(deltaPos, pos)
	}
	if err := scanner.Err(); err != nil {
		return nil, err
	}
	return deltaPos, nil
}
