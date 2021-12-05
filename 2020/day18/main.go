package main

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"log"
	"regexp"
	"strconv"
)

// TODO: Clean up further

func main() {
	numbers, err := ReadLines("input")
	if err != nil {
		log.Fatalln(err)
	}
	fmt.Println("1:", EvalFile(numbers, false))
	fmt.Println("2:", EvalFile(numbers, true))
}

var pattern = regexp.MustCompile(`(\([^()]+\))`)
var addpattern = regexp.MustCompile(`(\d+\+\d+)`)

func EvalFile(numbers [][]byte, plus bool) (sum int) {
	numcpy := make([][]byte, len(numbers))
	for i := range numcpy {
		numcpy[i] = make([]byte, len(numbers[i]))
		copy(numcpy[i], numbers[i])
	}
	for _, l := range numcpy {
		sum += EvalLine(l, plus)
	}
	return
}

func EvalLine(line []byte, plus bool) (sum int) {
	for {
		ixs := pattern.FindSubmatchIndex(line)
		if ixs != nil {
			var end []byte
			if !plus {
				end = append(EvalExpr(line[ixs[0]+1:ixs[1]-1]), line[ixs[1]:]...)
			} else {
				end = append(EvalExprPlus(line[ixs[0]+1:ixs[1]-1]), line[ixs[1]:]...)
			}
			line = append(line[:ixs[0]], end...)
		} else {
			break
		}
	}
	var err error
	if !plus {
		sum, err = strconv.Atoi(string(EvalExpr(line)))
	} else {
		sum, err = strconv.Atoi(string(EvalExprPlus(line)))
	}
	if err != nil {
		panic(err)
	}
	return
}

// Explicitly calls EvalExpr with all additions before evaluaing as normal
func EvalExprPlus(line []byte) []byte {
	for {
		ixs := addpattern.FindSubmatchIndex(line)
		if ixs != nil {
			end := append(EvalExpr(line[ixs[0]:ixs[1]]), line[ixs[1]:]...)
			line = append(line[:ixs[0]], end...)
		} else {
			break
		}
	}
	return EvalExpr(line)
}

func SplitWords(line []byte) [][]byte {
	words := make([][]byte, 0)
	acc := make([]byte, 0)
	for i := 0; i < len(line); i++ {
		switch line[i] {
		case '+', '*':
			words = append(words, acc)
			words = append(words, line[i:i+1])
			acc = []byte{}
		default:
			acc = append(acc, line[i])
		}
	}
	words = append(words, acc)
	return words
}

func EvalExpr(line []byte) []byte {
	words := SplitWords(line)
	sum, err := strconv.Atoi(string(words[0]))
	if err != nil {
		panic(err)
	}
	for i := 1; i < len(words); i += 2 {
		m, err := strconv.Atoi(string(words[i+1]))
		if err != nil {
			panic(err)
		}
		switch words[i][0] {
		case '+':
			sum += m
		case '*':
			sum *= m
		}
	}
	return []byte(strconv.Itoa(sum))
}

func ReadLines(filename string) ([][]byte, error) {
	content, err := ioutil.ReadFile(filename)
	if err != nil {
		return nil, err
	}
	trimcontent := bytes.TrimRight(content, "\n")
	trimcontent = bytes.ReplaceAll(trimcontent, []byte{' '}, nil)
	numbers := bytes.Split([]byte(trimcontent), []byte{'\n'})
	return numbers, nil
}
