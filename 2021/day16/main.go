package main

import (
	"bytes"
	"fmt"
	"io"
	"io/ioutil"
	"log"
	"strconv"
)

func main() {
	bufferData, err := ReadBITS("input")
	if err != nil {
		log.Fatalln(err)
	}
	fmt.Println("1:", VersionSum(bytes.NewBuffer(bufferData)))
	fmt.Println("2:", ReadPacket(bytes.NewBuffer(bufferData)))
}

func VersionSum(buffer *bytes.Buffer) int {
	versionSum := ReadN(buffer, 3)
	typeID := ReadN(buffer, 3)
	switch typeID {
	case 4:
		for {
			continueMarker := ReadN(buffer, 1)
			_ = ReadN(buffer, 4)
			if continueMarker == 0 {
				break
			}
		}
	default:
		lengthTypeID := ReadN(buffer, 1)
		switch lengthTypeID {
		case 0:
			length := ReadN(buffer, 15)
			startLen := buffer.Len()
			for (startLen - buffer.Len()) < length {
				versionSum += VersionSum(buffer)
			}
		case 1:
			length := ReadN(buffer, 11)
			for i := 0; i < length; i++ {
				versionSum += VersionSum(buffer)
			}
		}
	}
	return versionSum
}

func ReadPacket(buffer *bytes.Buffer) int {
	_ = ReadN(buffer, 3)
	typeID := ReadN(buffer, 3)
	switch typeID {
	case 0:
		return ParseOperand(buffer, func(e1, e2 int) int { return e1 + e2 })
	case 1:
		return ParseOperand(buffer, func(e1, e2 int) int { return e1 * e2 })
	case 2:
		return ParseOperand(buffer, func(e1, e2 int) int { return Min(e1, e2) })
	case 3:
		return ParseOperand(buffer, func(e1, e2 int) int { return Max(e1, e2) })
	case 4:
		var num int
		for {
			continueMarker := ReadN(buffer, 1)
			num <<= 4
			num += ReadN(buffer, 4)
			if continueMarker == 0 {
				break
			}
		}
		return num
	case 5:
		return ParseOperand(buffer, func(e1, e2 int) int { return IntBool(e1 > e2) })
	case 6:
		return ParseOperand(buffer, func(e1, e2 int) int { return IntBool(e1 < e2) })
	case 7:
		return ParseOperand(buffer, func(e1, e2 int) int { return IntBool(e1 == e2) })
	}
	panic("Unreachable")
}

func ParseOperand(buffer *bytes.Buffer, compFunc func(int, int) int) int {
	var operands []int
	lengthTypeID := ReadN(buffer, 1)
	switch lengthTypeID {
	case 0:
		length := ReadN(buffer, 15)
		startLen := buffer.Len()
		for (startLen - buffer.Len()) < length {
			operands = append(operands, ReadPacket(buffer))
		}
	case 1:
		length := ReadN(buffer, 11)
		for i := 0; i < length; i++ {
			operands = append(operands, ReadPacket(buffer))
		}
	}
	var ret = operands[0]
	for i := 1; i < len(operands); i++ {
		ret = compFunc(ret, operands[i])
	}
	return ret
}

func ReadN(buffer *bytes.Buffer, n int64) int {
	limitedReader := io.LimitReader(buffer, n)
	data, err := ioutil.ReadAll(limitedReader)
	if err != nil {
		panic(err)
	}
	num, err := strconv.ParseInt(string(data), 2, 64)
	if err != nil {
		panic(err)
	}
	return int(num)
}

func IntBool(bit bool) int {
	if bit {
		return 1
	}
	return 0
}

func Max(e1, e2 int) int {
	if e1 > e2 {
		return e1
	}
	return e2
}

func Min(e1, e2 int) int {
	if e1 < e2 {
		return e1
	}
	return e2
}

func ReadBITS(filename string) ([]byte, error) {
	content, err := ioutil.ReadFile(filename)
	if err != nil {
		return nil, err
	}
	characters := bytes.TrimRight(content, "\n")
	var hexToBinary = map[byte]string{
		'0': "0000", '1': "0001", '2': "0010", '3': "0011",
		'4': "0100", '5': "0101", '6': "0110", '7': "0111",
		'8': "1000", '9': "1001", 'A': "1010", 'B': "1011",
		'C': "1100", 'D': "1101", 'E': "1110", 'F': "1111",
	}
	var bufferData []byte
	for _, b := range characters {
		bufferData = append(bufferData, hexToBinary[b]...)
	}
	return bufferData, nil
}
