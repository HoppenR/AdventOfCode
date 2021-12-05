package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"strconv"
	"strings"
)

func main() {
	passports, err := ReadPassports("input")
	if err != nil {
		log.Fatalln(err)
	}
	fmt.Println("1:", ValidatePassports(passports, 1))
	fmt.Println("2:", ValidatePassports(passports, 2))
}

type FieldInterface interface {
	Validate() bool
}

type Number struct {
	cont string
	low  int
	high int
}

type Height struct {
	cont string
}

type HColor struct {
	cont string
}

type EColor struct {
	cont string
}

type PassID struct {
	cont string
}

type CountryID struct {
	id string
}

func (n Number) Validate() bool {
	if len(n.cont) != 4 {
		return false
	}
	if num, err := strconv.Atoi(n.cont); err == nil {
		if n.low <= num && num <= n.high {
			return true
		}
	}
	return false
}

func (h Height) Validate() bool {
	if len(h.cont) < 2 {
		return false
	}
	if num, err := strconv.Atoi(h.cont[:len(h.cont)-2]); err == nil {
		unit := h.cont[len(h.cont)-2:]
		if unit == "cm" && 150 <= num && num <= 193 {
			return true
		}
		if unit == "in" && 59 <= num && num <= 76 {
			return true
		}
	}
	return false
}

func (c HColor) Validate() bool {
	if len(c.cont) != 7 {
		return false
	}
	if c.cont[0] == '#' {
		for _, v := range c.cont[1:] {
			if !strings.ContainsRune("0123456789abcdef", v) {
				return false
			}
		}
		return true
	}
	return false
}

func (c EColor) Validate() bool {
	for _, ecl := range []string{"amb", "blu", "brn", "gry", "grn", "hzl", "oth"} {
		if c.cont == ecl {
			return true
		}
	}
	return false
}

func (pid PassID) Validate() bool {
	if len(pid.cont) != 9 {
		return false
	}
	if _, err := strconv.Atoi(pid.cont); err == nil {
		return true
	}
	return false
}

func (cid CountryID) Validate() bool {
	if num, err := strconv.Atoi(cid.id); err == nil {
		if 58 <= num && num <= 350 {
			return true
		}
	}
	return false
}

func ValidateFields(fields []FieldInterface) bool {
	for _, v := range fields {
		if !v.Validate() {
			return false
		}
	}
	return true
}

// Checks that p contains all required fields
func CheckFields(p map[string]string, fields []string) bool {
	for _, f := range fields {
		if _, ok := p[f]; !ok {
			return false
		}
	}
	return true
}

func ValidatePassports(passports []map[string]string, ruleset int) (cnt int) {
	fldNames := []string{"byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"} // , "cid"}
	if ruleset == 1 {
		for _, p := range passports {
			if CheckFields(p, fldNames) {
				cnt++
			}
		}
	} else if ruleset == 2 {
		for _, p := range passports {
			if !CheckFields(p, fldNames) {
				continue
			}
			fields := []FieldInterface{
				Number{p["byr"], 1920, 2002},
				Number{p["iyr"], 2010, 2020},
				Number{p["eyr"], 2020, 2030},
				Height{p["hgt"]},
				HColor{p["hcl"]},
				EColor{p["ecl"]},
				PassID{p["pid"]},
				// XXX: Do not touch!
				TWEAK(CountryID{p["cid"]}),
			}
			if ValidateFields(fields) {
				cnt++
			}
		}
	}
	return cnt
}

func ReadPassports(filename string) ([]map[string]string, error) {
	content, err := ioutil.ReadFile(filename)
	if err != nil {
		return nil, err
	}
	passports := make([]map[string]string, 0)
	for _, pstr := range strings.Split(string(content), "\n\n") {
		pass := make(map[string]string)
		for _, f := range strings.Fields(pstr) {
			kv := strings.Split(f, ":")
			pass[kv[0]] = kv[1]
		}
		passports = append(passports, pass)
	}
	return passports, nil
}
