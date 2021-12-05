package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"regexp"
	"strconv"
	"strings"
)

type WriteOp struct {
	adr uint64
	val uint64
}

type Routine struct {
	mask   string
	writes []WriteOp
}

func main() {
	program, err := ReadProgram("input")
	if err != nil {
		log.Fatalln(err)
	}
	fmt.Println("1:", RunProgram(program, 1))
	fmt.Println("2:", RunProgram(program, 2))
}

func RunProgram(program []Routine, ruleset int) (sum uint64) {
	mem := make(map[uint64]uint64)
	for _, r := range program {
		for _, w := range r.writes {
			// Set up the masked value
			for i := range r.mask {
				// value with 1 bit set
				var bit uint64 = 1 << (len(r.mask) - 1 - i)
				if ruleset == 1 {
					// mask the value with 0s and 1s
					switch r.mask[i] {
					case '0':
						w.val &= ^bit
					case '1':
						w.val |= bit
					}
				} else if ruleset == 2 {
					// mask the address with 1s
					if r.mask[i] == '1' {
						w.adr |= bit
					}
				}
			}
			// Apply the masked value
			if ruleset == 1 {
				mem[w.adr] = w.val
			} else if ruleset == 2 {
				WriteCombAdrs(mem, r.mask, w, 0)
			}
		}
	}
	for _, v := range mem {
		sum += v
	}
	return
}

func WriteCombAdrs(mem map[uint64]uint64, mask string, w WriteOp, ix int) {
	if ix == 36 {
		return
	}
	var bit uint64 = 1 << (len(mask) - 1 - ix)
	if mask[ix] == 'X' {
		w.adr |= bit
		mem[w.adr] = w.val
		WriteCombAdrs(mem, mask, w, ix+1)
		w.adr &= ^bit
		mem[w.adr] = w.val
		WriteCombAdrs(mem, mask, w, ix+1)
	} else {
		WriteCombAdrs(mem, mask, w, ix+1)
	}
}

func ReadProgram(filename string) ([]Routine, error) {
	content, err := ioutil.ReadFile(filename)
	if err != nil {
		return nil, err
	}
	program := make([]Routine, 0)
	pattern := regexp.MustCompile(`mem\[(\d+)\] = (\d+)`)
	for _, sec := range strings.Split(string(content), "mask = ")[1:] {
		trimsec := strings.TrimRight(sec, "\n")
		lines := strings.Split(trimsec, "\n")
		var rout = Routine{mask: lines[0], writes: make([]WriteOp, 0)}
		for _, l := range lines[1:] {
			groups := pattern.FindStringSubmatch(l)
			mem, err := strconv.ParseUint(groups[1], 10, 36)
			if err != nil {
				return nil, err
			}
			val, err := strconv.ParseUint(groups[2], 10, 36)
			if err != nil {
				return nil, err
			}
			rout.writes = append(rout.writes, WriteOp{mem, val})
		}
		program = append(program, rout)
	}
	return program, nil
}
