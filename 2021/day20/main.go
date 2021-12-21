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

type Image struct {
	pixels map[Pixel]struct{}
	min    int
	max    int
	algo   string
}

var compOffsets = []Pixel{
	{1, 1}, {0, 1}, {-1, 1},
	{1, 0}, {0, 0}, {-1, 0},
	{1, -1}, {0, -1}, {-1, -1},
}

func main() {
	image, err := GetImageWithAlgo("input")
	if err != nil {
		log.Fatalln(err)
	}
	fmt.Println("1:", Enhance(image, 2))
	fmt.Println("2:", Enhance(image, 50-2))
}

func Enhance(image *Image, times int) int {
	pixelsAreOn := true
	isAlternating := (image.algo[0] == '#')
	for i := 0; i < times; i++ {
		Upscale(image, pixelsAreOn, isAlternating)
		if isAlternating {
			pixelsAreOn = !pixelsAreOn
		}
	}
	return len(image.pixels)
}

func Upscale(image *Image, pixelsAreOn, changeOutput bool) {
	newPixels := make(map[Pixel]struct{})
	for y := image.min - 1; y <= image.max+1; y++ {
		for x := image.min - 1; x <= image.max+1; x++ {
			newColor := image.algo[PixelToInt(Pixel{x, y}, image, pixelsAreOn)]
			lightPx := newColor == '#'
			if changeOutput {
				lightPx = !lightPx
			}
			if lightPx == pixelsAreOn {
				newPixels[Pixel{x, y}] = struct{}{}
			}
		}
	}
	image.min--
	image.max++
	image.pixels = newPixels
	return
}

func PixelToInt(pixel Pixel, image *Image, pixelsAreOn bool) (num int) {
	for i, Δc := range compOffsets {
		if _, ok := image.pixels[Pixel{pixel.x + Δc.x, pixel.y + Δc.y}]; ok == pixelsAreOn {
			num += 1 << i
		}
	}
	return
}

func GetImageWithAlgo(filename string) (*Image, error) {
	file, err := os.Open(filename)
	if err != nil {
		return nil, err
	}
	defer file.Close()
	scanner := bufio.NewScanner(file)
	scanner.Scan()
	image := &Image{
		pixels: make(map[Pixel]struct{}),
		algo:   scanner.Text(),
	}
	scanner.Scan()
	for row := 0; scanner.Scan(); row++ {
		for col, color := range scanner.Text() {
			if color == '#' {
				image.pixels[Pixel{col, row}] = struct{}{}
				if col > image.max {
					image.max = col
				}
			}
		}
	}
	return image, nil
}
