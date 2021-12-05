package main

import (
	"errors"
	"fmt"
	"io/ioutil"
	"log"
	"strconv"
	"strings"
)

type (
	acc int
	ptr int
	nop int
)

type Instr struct {
	opcode interface{}
	seen   bool
}

func main() {
	prg, err := ReadProgram("input")
	if err != nil {
		log.Fatalln(err)
	}
	ans1, _ := TryRun(prg)
	fmt.Println("1:", ans1)
	ans2, err := FindFaultyInstr(prg)
	if err != nil {
		log.Fatalln(err)
	}
	fmt.Println("2:", ans2)
}

// Returns before loop or on exit with the latest acc and ptr values
func TryRun(prg []Instr) (acc, ptr) {
	defer ResetSeen(prg)
	var a acc
	var p ptr
	for int(p) < len(prg) {
		if prg[p].seen {
			return a, p
		}
		prg[p].seen = true
		switch v := prg[p].opcode.(type) {
		case acc:
			a += v
			p += 1
		case ptr:
			p += v
		case nop:
			p += 1
		}
	}
	return a, p
}

func FindFaultyInstr(prg []Instr) (acc, error) {
	for i := range prg {
		switch prg[i].opcode.(type) {
		case acc:
			continue
		}
		FlipInstr(prg, i)
		a, p := TryRun(prg)
		FlipInstr(prg, i)
		if int(p) == len(prg) {
			return a, nil
		}
	}
	return 0, errors.New("Could not reach the end of the program")
}

func FlipInstr(prg []Instr, i int) {
	switch v := prg[i].opcode.(type) {
	case ptr:
		prg[i] = Instr{opcode: nop(v)}
	case nop:
		prg[i] = Instr{opcode: ptr(v)}
	}
}

func ResetSeen(prg []Instr) {
	for i := range prg {
		prg[i].seen = false
	}
}

func ReadProgram(filename string) ([]Instr, error) {
	content, err := ioutil.ReadFile(filename)
	if err != nil {
		return nil, err
	}
	trimcontent := strings.TrimRight(string(content), "\n")
	lines := strings.Split(trimcontent, "\n")
	var prg []Instr
	for _, l := range lines {
		var instr Instr
		parts := strings.Fields(l)
		value, err := strconv.Atoi(parts[1])
		if err != nil {
			return nil, err
		}
		switch parts[0] {
		case "acc":
			instr.opcode = acc(value)
		case "jmp":
			instr.opcode = ptr(value)
		case "nop":
			instr.opcode = nop(value)
		default:
			return nil, fmt.Errorf("%s: %s\n", "Invalid opcode", parts[0])
		}
		prg = append(prg, instr)
	}
	return prg, nil
}
