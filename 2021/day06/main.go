package main

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"log"
	"strconv"
)

func main() {
	fishes, err := ParseFishTimers("input")
	if err != nil {
		log.Fatalln(err)
	}
	fmt.Println("1:", Iterate(fishes, 80))
	fmt.Println("2:", Iterate(fishes, 256-80))
}

func Iterate(t *[9]int, iterations int) (sum int) {
	for i := 1; i <= iterations; i++ {
		t[0], t[1], t[2], t[3], t[4], t[5], t[6], t[7], t[8] =
			t[1], t[2], t[3], t[4], t[5], t[6], t[0]+t[7], t[8], t[0]
	}
	for _, v := range t {
		sum += v
	}
	return
}

func ParseFishTimers(filename string) (*[9]int, error) {
	content, err := ioutil.ReadFile(filename)
	if err != nil {
		return nil, err
	}
	trimcontent := bytes.TrimRight(content, "\n")
	var timers [9]int
	for _, l := range bytes.Split(trimcontent, []byte(",")) {
		num, err := strconv.Atoi(string(l))
		if err != nil {
			return nil, err
		}
		timers[num]++
	}
	return &timers, nil
}
