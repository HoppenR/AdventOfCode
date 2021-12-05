package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"regexp"
	"strconv"
	"strings"
)

// # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
// NOTE: I probably don't even want to clean this up... good luck reading this.
// # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

type Ticket struct {
	values []int
}

type Field struct {
	lows  []int
	highs []int
	name  string
}

func main() {
	fields, tickets, myticket, err := ReadInts("input")
	if err != nil {
		log.Fatalln(err)
	}
	_, ans1 := ValidateTickets(fields, tickets)
	fmt.Println("1:", ans1)
	fmt.Println("2:", DetermineFields(fields, tickets, myticket))
}

func ValidateTickets(ranges []Field, tickets []Ticket) ([]Ticket, int) {
	valids := make([]Ticket, 0)
	var inval int
	for _, t := range tickets {
		if n, ok := ValidTicket(ranges, t); !ok {
			inval += n
		} else {
			valids = append(valids, t)
		}
	}
	return valids, inval
}

func ValidTicket(fields []Field, t Ticket) (int, bool) {
	for _, n := range t.values {
		var valid = false
		for _, f := range fields {
			for i := 0; i < 2; i++ {
				if n >= f.lows[i] && n <= f.highs[i] {
					valid = true
				}
			}
		}
		if !valid {
			return n, false
		}
	}
	return 0, true
}

func DetermineFields(fields []Field, tickets []Ticket, myticket Ticket) int {
	valids, _ := ValidateTickets(fields, tickets)
	allcandidates := make(map[string][]int, 0)
	for _, f := range fields {
		candidates := make([]int, 0)
		for i := 0; i < len(fields); i++ {
			var canbe bool = true
			for _, t := range valids {
				if (t.values[i] >= f.lows[0] && t.values[i] <= f.highs[0]) ||
					(t.values[i] >= f.lows[1] && t.values[i] <= f.highs[1]) {
				} else {
					canbe = false
				}
			}
			if canbe {
				candidates = append(candidates, i)
			}
		}
		allcandidates[f.name] = candidates
	}
	determined := make(map[string]int, 0)
	for len(determined) < len(fields) {
		for k, v := range allcandidates {
			if len(v) == 1 {
				determined[k] = allcandidates[k][0]
				delete(allcandidates, k)
				for w := range allcandidates {
					for q := range allcandidates[w] {
						if allcandidates[w][q] == v[0] {
							allcandidates[w] = remove(allcandidates[w], q)
							break
						}
					}
				}
				break
			}
		}
	}
	sum := 1
	for k, v := range determined {
		if len(k) < 9 {
			continue
		}
		if k[:9] == "departure" {
			sum *= myticket.values[v]
		}
	}
	return sum
}

func remove(s []int, i int) []int {
	s[len(s)-1], s[i] = s[i], s[len(s)-1]
	return s[:len(s)-1]
}

func ReadInts(filename string) ([]Field, []Ticket, Ticket, error) {
	file, err := os.Open(filename)
	if err != nil {
		return nil, nil, Ticket{}, err
	}
	defer file.Close()
	pattern := regexp.MustCompile(`^(.+): (\d+)-(\d+) or (\d+)-(\d+)$`)
	fields := make([]Field, 0)
	// RANGES
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		if scanner.Text() == "" {
			scanner.Scan()
			break
		}
		groups := pattern.FindStringSubmatch(scanner.Text())[1:]
		var f Field
		f.name = groups[0]
		for i := 1; i <= 3; i += 2 {
			low, err := strconv.Atoi(groups[i])
			if err != nil {
				return nil, nil, Ticket{}, err
			}
			high, err := strconv.Atoi(groups[i+1])
			if err != nil {
				return nil, nil, Ticket{}, err
			}
			f.lows = append(f.lows, low)
			f.highs = append(f.highs, high)
		}
		fields = append(fields, f)
	}
	// MY TICKET
	var myticket Ticket
	for scanner.Scan() {
		if scanner.Text() == "" {
			scanner.Scan()
			break
		}
		for _, v := range strings.Split(scanner.Text(), ",") {
			num, err := strconv.Atoi(v)
			if err != nil {
				return nil, nil, Ticket{}, err
			}
			myticket.values = append(myticket.values, num)
		}
	}
	// NEARBY TICKETS
	tickets := make([]Ticket, 0)
	for scanner.Scan() {
		var t Ticket
		for _, v := range strings.Split(scanner.Text(), ",") {
			num, err := strconv.Atoi(v)
			if err != nil {
				return nil, nil, Ticket{}, err
			}
			t.values = append(t.values, num)
		}
		tickets = append(tickets, t)
	}
	if err := scanner.Err(); err != nil {
		return nil, nil, Ticket{}, err
	}
	return fields, tickets, myticket, nil
}
