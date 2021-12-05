package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"regexp"
	"sort"
	"strings"

	"github.com/scylladb/go-set/strset"
)

// ATI = Allergen -> ingredients.
// Ingredients is a slice of each line (as a set) where the allergen appears, ex
// ati[dairy] -> [Set("mxmxvkd", "sqjhc"), Set("trh", "mxmxvkd")]
// ati[fish]  -> [Set("mxmxvkd", "sqjhc"), Set("sqjhc", "sbzzf")]
type ATI map[string][]*strset.Set

type Ingredient struct {
	name     string
	allergen string
}

func main() {
	// Ingredients only contain one single instance of each set, ati does not
	ati, ingredients, err := ReadFoods("input")
	if err != nil {
		log.Fatalln(err)
	}
	fmt.Println("1:", NumSafe(ati, ingredients))
	fmt.Println("2:", DangerousIngredients(ati))
}

func NumSafe(ati ATI, ingredients []*strset.Set) (cnt int) {
	bad := strset.New()
	for _, lines := range ati {
		bad.Merge(strset.Intersection(lines...))
	}
	for _, in := range ingredients {
		cnt += strset.Difference(in, bad).Size()
	}
	return
}

func DangerousIngredients(ati ATI) string {
	// dangerous[allergen] -> Set(candidate1, candidate2, ...)
	dangerous := make(map[string]*strset.Set)
	for allergen, lines := range ati {
		dangerous[allergen] = strset.Intersection(lines...)
	}
	var assigned []Ingredient
	for len(dangerous) > 0 {
		for allergen, candidates := range dangerous {
			if candidates.Size() == 1 {
				name := candidates.Pop()
				assigned = append(assigned, Ingredient{name, allergen})
				delete(dangerous, allergen)
				for i := range dangerous {
					dangerous[i].Remove(name)
				}
			}
		}
	}
	sort.Slice(assigned, func(i, j int) bool {
		return assigned[i].allergen < assigned[j].allergen
	})
	var items []string
	for _, in := range assigned {
		items = append(items, in.name)
	}
	return strings.Join(items, ",")
}

func ReadFoods(filename string) (ATI, []*strset.Set, error) {
	file, err := os.Open(filename)
	if err != nil {
		return nil, nil, err
	}
	defer file.Close()
	scanner := bufio.NewScanner(file)
	ati := make(ATI)
	var ingredients []*strset.Set
	pattern := regexp.MustCompile(`(.+) \(contains (.+)\)`)
	for scanner.Scan() {
		groups := pattern.FindStringSubmatch(scanner.Text())
		line := strset.New(strings.Fields(groups[1])...)
		for _, allergen := range strings.Split(groups[2], ", ") {
			ati[allergen] = append(ati[allergen], line)
		}
		ingredients = append(ingredients, line)
	}
	if err := scanner.Err(); err != nil {
		return nil, nil, err
	}
	return ati, ingredients, nil
}
