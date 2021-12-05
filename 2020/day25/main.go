package main

import (
	"bufio"
	"errors"
	"fmt"
	"log"
	"os"
	"strconv"
)

func main() {
	cardPKey, doorPKey, err := ReadRules("input")
	if err != nil {
		log.Fatalln(err)
	}
	ans1, err := EncryptionKey(cardPKey, doorPKey)
	if err != nil {
		log.Fatalln(err)
	}
	fmt.Println("1:", ans1)
}

func EncryptionKey(cardPKey, doorPKey int) (int, error) {
	loopCard := LoopSize(7, cardPKey)
	loopDoor := LoopSize(7, doorPKey)
	key1 := Exp(cardPKey, loopDoor)
	key2 := Exp(doorPKey, loopCard)
	if key1 == key2 {
		return key1, nil
	} else {
		return 0, errors.New("The encryption keys do not match")
	}
}

func LoopSize(subjn int, publicKey int) (loops int) {
	for Exp(7, loops) != publicKey {
		loops++
	}
	return
}

func Exp(n int, e int) int {
	if e == 0 {
		return 1
	} else if e%2 == 0 {
		return Exp((n*n)%20201227, e/2)
	} else {
		return (n * Exp(n, e-1)) % 20201227
	}
}

func ReadRules(filename string) (int, int, error) {
	file, err := os.Open(filename)
	if err != nil {
		return 0, 0, err
	}
	defer file.Close()
	scanner := bufio.NewScanner(file)
	scanner.Scan()
	cardPKey, err := strconv.Atoi(scanner.Text())
	scanner.Scan()
	doorPKey, err := strconv.Atoi(scanner.Text())
	if err := scanner.Err(); err != nil {
		return 0, 0, err
	}
	return cardPKey, doorPKey, nil
}
