package main

import (
	"github.com/stretchr/testify/assert"
	"testing"
)

var testPatterns = []SignalPattern{
	{
		input:  [10]string{"be", "cfbegad", "cbdgef", "fgaecd", "cgeb", "fdcge", "agebfd", "fecdb", "fabcd", "edb"},
		output: [4]string{"fdgacbe", "cefdb", "cefbgd", "gcbe"},
	},
	{
		input:  [10]string{"edbfga", "begcd", "cbg", "gc", "gcadebf", "fbgde", "acbgfd", "abcde", "gfcbed", "gfec"},
		output: [4]string{"fcgedb", "cgb", "dgebacf", "gc"},
	},
	{
		input:  [10]string{"fgaebd", "cg", "bdaec", "gdafb", "agbcfd", "gdcbef", "bgcad", "gfac", "gcb", "cdgabef"},
		output: [4]string{"cg", "cg", "fdcagb", "cbg"},
	},
	{
		input:  [10]string{"fbegcd", "cbd", "adcefb", "dageb", "afcb", "bc", "aefdc", "ecdab", "fgdeca", "fcdbega"},
		output: [4]string{"efabcd", "cedba", "gadfec", "cb"},
	},
	{
		input:  [10]string{"aecbfdg", "fbg", "gf", "bafeg", "dbefa", "fcge", "gcbea", "fcaegb", "dgceab", "fcbdga"},
		output: [4]string{"gecf", "egdcabf", "bgf", "bfgea"},
	},
	{
		input:  [10]string{"fgeab", "ca", "afcebg", "bdacfeg", "cfaedg", "gcfdb", "baec", "bfadeg", "bafgc", "acf"},
		output: [4]string{"gebdcfa", "ecba", "ca", "fadegcb"},
	},
	{
		input:  [10]string{"dbcfg", "fgd", "bdegcaf", "fgec", "aegbdf", "ecdfab", "fbedc", "dacgb", "gdcebf", "gf"},
		output: [4]string{"cefg", "dcbef", "fcge", "gbcadfe"},
	},
	{
		input:  [10]string{"bdfegc", "cbegaf", "gecbf", "dfcage", "bdacg", "ed", "bedf", "ced", "adcbefg", "gebcd"},
		output: [4]string{"ed", "bcgafe", "cdgba", "cbgef"},
	},
	{
		input:  [10]string{"egadfb", "cdbfeg", "cegd", "fecab", "cgb", "gbdefca", "cg", "fgcdab", "egfdb", "bfceg"},
		output: [4]string{"gbdfcae", "bgc", "cg", "cgb"},
	},
	{
		input:  [10]string{"gcafb", "gcf", "dcaebfg", "ecagb", "gf", "abcdeg", "gaef", "cafbge", "fdbac", "fegbdc"},
		output: [4]string{"fgae", "cfgab", "fg", "bagce"},
	},
}

func TestPart1(t *testing.T) {
	assert.Equal(t, 26, CountUnique(testPatterns))
}

func TestPart2(t *testing.T) {
}
