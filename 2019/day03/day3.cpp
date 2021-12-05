#include <climits>
#include <iostream>
#include <map>
#include <set>
#include <sstream>
#include <stdexcept>
#include <string>
#include <time.h>
#include <vector>
#define PART1 1
#define PART2 1

using DirectionInfo = std::pair<char, int>;
using Directions2d = std::vector<DirectionInfo>;
using Directions3d = std::vector<Directions2d>;
using Point = std::pair<int, int>;
using Points = std::set<Point>;
static const std::map<char, Point> movement = {
	// Represents the changes in X, Y for 1 step in every direction
	{ 'U', { 0, -1 } },
	{ 'L', { 1, 0 } },
	{ 'D', { 0, 1 } },
	{ 'R', { -1, 0 } },
};

static void operator+=(Point& lhs, const Point& rhs) {
	lhs.first += rhs.first;
	lhs.second += rhs.second;
}

int find_closest_intersection(Directions3d Wires) {
	Points visitedPoints;
	int mindistance = INT_MAX;
	bool firstWire = true;
	for (Directions2d Wire : Wires) {
		Point posxy = { 0, 0 };
		for (DirectionInfo curDirectionInfo : Wire) {
			const Point curMovement = movement.at(curDirectionInfo.first);
			for (int i = 0; i < curDirectionInfo.second; i++) {
				posxy += curMovement;
				if (firstWire) {
					visitedPoints.insert(posxy);
				} else if (visitedPoints.find(posxy) != visitedPoints.end()) {
					const int dist = abs(posxy.first) + abs(posxy.second);
					mindistance = std::min(mindistance, dist);
				}
			}
		}
		firstWire = false;
	}
	return mindistance;
}

int find_best_signal(Directions3d Wires) {
	std::map<Point, int> visitedPoints;
	// Maps a Point to the amount of steps there
	int mindistance = INT_MAX;
	bool firstWire = true;
	for (Directions2d Wire : Wires) {
		Point posxy = { 0, 0 };
		int steps = 0;
		for (DirectionInfo curDirectionInfo : Wire) {
			const Point curMovement = movement.at(curDirectionInfo.first);
			for (int i = 0; i < curDirectionInfo.second; i++) {
				posxy += curMovement;
				steps += 1;
				if (firstWire) {
					visitedPoints.insert(std::make_pair(posxy, steps));
				} else if (visitedPoints.find(posxy) != visitedPoints.end()) {
					const int dist = visitedPoints.at(posxy) + steps;
					mindistance = std::min(mindistance, dist);
				}
			}
		}
		firstWire = false;
	}
	return mindistance;
}

int main(void) {
	const time_t start = clock();
	Directions3d Wires;
	for (std::string line; std::getline(std::cin, line);) {
		Directions2d Wire;
		std::stringstream lineStream(line);
		for (std::string substring; std::getline(lineStream, substring, ',');) {
			Wire.push_back(
				DirectionInfo(substring[0], std::stoi(substring.substr(1))));
		}
		Wires.push_back(Wire);
	}
#if PART1
	std::cout << "p1: " << find_closest_intersection(Wires) << '\n';
#endif // PART1
#if PART2
	std::cout << "p2: " << find_best_signal(Wires) << '\n';
#endif // PART2
	const time_t end = clock();
	std::cout << "time: " << difftime(end, start) / CLOCKS_PER_SEC << "s\n";
	return EXIT_SUCCESS;
}
