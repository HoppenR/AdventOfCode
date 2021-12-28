package main

import (
	"fmt"
	"log"
	"os"
)

type PlayerStats struct {
	p1, p2 int
}

var PossibleRollCounts3d3 = map[int]int{
	3: 1, 4: 3, 5: 6, 6: 7, 7: 6, 8: 3, 9: 1,
}

func main() {
	startPos, err := ReadFile("input")
	if err != nil {
		log.Fatalln(err)
	}
	fmt.Println("1:", RollDeterministic(startPos))
	fmt.Println("2:", RollQuantum(startPos))
}

func RollDeterministic(startPos *PlayerStats) int {
	var scores [2]int
	var pos = [2]int{startPos.p1, startPos.p2}
	for rolls := 0; ; {
		for player := 0; player < 2; player++ {
			pos[player] += 3*rolls + 6
			rolls += 3
			pos[player] = ((pos[player] - 1) % 10) + 1
			scores[player] += pos[player]
			if scores[player] >= 1000 {
				return rolls * scores[player^1]
			}
		}
	}
}

func RollQuantum(startPos *PlayerStats) int {
	DP := make(map[int]PlayerStats)
	wins := RollGeneration(1, PlayerStats{}, *startPos, DP)
	if wins.p1 > wins.p2 {
		return wins.p1
	}
	return wins.p2
}

func RollGeneration(player int, score, pos PlayerStats, DP map[int]PlayerStats) PlayerStats {
	if wins, ok := DP[Hash(player, score, pos)]; ok {
		return wins
	}
	var wins PlayerStats
	for roll, count := range PossibleRollCounts3d3 {
		newScore, newPos := score, pos
		switch player {
		case 1:
			newPos.p1 = (pos.p1+roll-1)%10 + 1
			newScore.p1 += newPos.p1
			if newScore.p1 >= 21 {
				wins.p1 += count
				continue
			}
		case 2:
			newPos.p2 = (pos.p2+roll-1)%10 + 1
			newScore.p2 += newPos.p2
			if newScore.p2 >= 21 {
				wins.p2 += count
				continue
			}
		}
		newWins := RollGeneration((player-1)^1+1, newScore, newPos, DP)
		wins.p1 += newWins.p1 * count
		wins.p2 += newWins.p2 * count
	}
	DP[Hash(player, score, pos)] = wins
	return wins
}

func Hash(player int, score, pos PlayerStats) int {
	return player*1e8 + score.p1*1e6 + score.p2*1e4 + pos.p1*1e2 + pos.p2
}

func ReadFile(filename string) (*PlayerStats, error) {
	file, err := os.Open(filename)
	if err != nil {
		return nil, err
	}
	defer file.Close()
	var startPos PlayerStats
	_, err = fmt.Fscanf(
		file,
		"Player 1 starting position: %d\nPlayer 2 starting position: %d\n",
		&startPos.p1,
		&startPos.p2,
	)
	if err != nil {
		return nil, err
	}
	return &startPos, nil
}
