package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
)

type Pixel struct {
	x, y int
}

type Image map[Pixel]struct{}

var compOffsets = []Pixel{
	{1, 1}, {0, 1}, {-1, 1},
	{1, 0}, {0, 0}, {-1, 0},
	{1, -1}, {0, -1}, {-1, -1},
}

func main() {
	algo, image, err := GetImageWithAlgo("input")
	if err != nil {
		log.Fatalln(err)
	}
	fmt.Println("1:", Enhance(algo, image, 2))
	fmt.Println("2:", Enhance(algo, image, 50))
}

func Enhance(algo string, image Image, times int) int {
	inputIsOn := true
	isAlternating := algo[0] == '#'
	for i := 0; i < times; i++ {
		image = Upscale(algo, image, inputIsOn, isAlternating)
		if isAlternating {
			inputIsOn = !inputIsOn
		}
	}
	return len(image)
}

func Upscale(algo string, image Image, inputIsOn, changeOutput bool) Image{
	newImage := make(Image)
	var min Pixel
	var max Pixel
	for pixel := range image {
		if pixel.x < min.x {
			min.x = pixel.x
		}
		if pixel.y < min.y {
			min.y = pixel.y
		}
		if pixel.x > max.x {
			max.x = pixel.x
		}
		if pixel.y > max.y {
			max.y = pixel.y
		}
	}
	for y := min.y - 1; y <= max.y+1; y++ {
		for x := min.x - 1; x <= max.x+1; x++ {
			newColor := algo[PixelToInt(Pixel{x, y}, image, inputIsOn)]
			lightPx := newColor == '#'
			if changeOutput {
				lightPx = !lightPx
			}
			if lightPx == inputIsOn {
				newImage[Pixel{x, y}] = struct{}{}
			}
		}
	}
	return newImage
}

func PixelToInt(pixel Pixel, image Image, inputIsOn bool) (num int) {
	for i, Δc := range compOffsets {
		if _, ok := image[Pixel{pixel.x + Δc.x, pixel.y + Δc.y}]; ok == inputIsOn {
			num += 1 << i
		}
	}
	return
}

func GetImageWithAlgo(filename string) (string, Image, error) {
	file, err := os.Open(filename)
	if err != nil {
		return "", nil, err
	}
	defer file.Close()
	scanner := bufio.NewScanner(file)
	scanner.Scan()
	image := make(Image)
	algo := scanner.Text()
	scanner.Scan()
	for row := 0; scanner.Scan(); row++ {
		for col, color := range scanner.Text() {
			if color == '#' {
				image[Pixel{col, row}] = struct{}{}
			}
		}
	}
	return algo, image, nil
}
