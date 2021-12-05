#include <climits>
#include <iostream>
#include <map>
#include <math.h>
#include <set>
#include <stdexcept>
#include <string>
#include <time.h>
#define PART1 1
#define PART2 1

// Requires c++17 to compile because of the usage of structured bindings

using Asteroid = std::pair<ushort, ushort>;
using Asteroidset = std::set<Asteroid>;
using AsteroidsIter = Asteroidset::const_iterator;
using AngleToAsteroidSetMap = std::map<double, Asteroidset>;
using BestStationInfo = std::pair<Asteroid, ushort>;

double get_angle(const Asteroid& from, const Asteroid& to) {
	const double dx = to.first - from.first;
	const double dy = to.second - from.second;
	// Due to the y position increasing as we move south on the map, atan2
	// returns clockwise angle instead of counter clockwise as if would if y
	// position increases as we moved north on the map
	// (like a normal euclidean plane)
	double angle = atan2(dy, dx) * (180 / M_PI);
	angle += 90.0f; // align to "north" (south)
	// Angle can be between -90.0f and +270.0f here, this normalizes it to
	// between 0.0f and 359.9f
	angle = fmod(angle + 360, 360);
	return angle;
}

ushort count_visible_asteroids(const Asteroidset& AsterCoords,
							   const Asteroid& station) {
	std::set<double> seenAngles;
	for (const Asteroid& asteroid : AsterCoords) {
		if (station == asteroid)
			continue;
		seenAngles.insert(get_angle(station, asteroid));
	}
	return seenAngles.size();
}

BestStationInfo find_best_asteroid(const Asteroidset& AsterCoords) {
	// Returns a pair of the best Asteroid and the number of visible asteroids
	// from that location Where best asteroid is the one with the most visible
	// asteroids
	ushort highestOverviewCount = 0;
	Asteroid bestAsteroid;
	for (const Asteroid& asteroid : AsterCoords) {
		const ushort visCount = count_visible_asteroids(AsterCoords, asteroid);
		if (visCount > highestOverviewCount) {
			highestOverviewCount = visCount;
			bestAsteroid = asteroid;
		}
	}
	return std::make_pair(bestAsteroid, highestOverviewCount);
}

AngleToAsteroidSetMap* get_angles_map(const Asteroidset& AsterCoords,
									  const Asteroid& station) {
	// Returns a pointer to a newly allocated map
	// this map maps angles to a set of asteroids, all asteroids in one set
	// share the same angle.
	// The angle is calculated with the coordinates from the station parameter
	// and is relative to the x-axis
	AngleToAsteroidSetMap* anglesMap = new AngleToAsteroidSetMap;
	for (const Asteroid& asteroid : AsterCoords) {
		if (station == asteroid)
			continue;
		const double angle = get_angle(station, asteroid);
		if (anglesMap->find(angle) == anglesMap->end())
			anglesMap->insert(std::make_pair(angle, Asteroidset{}));
		anglesMap->at(angle).insert(asteroid);
	}
	return anglesMap;
}

AsteroidsIter find_closest_asteroid(AsteroidsIter iter, AsteroidsIter end,
									const Asteroid& station) {
	// Takes a set of asteroids that all share the same angle relative to the
	// station and returns the closest one, calculated in manhattan distance
	AsteroidsIter closest;
	ushort closestdist = USHRT_MAX;
	while (iter != end) {
		const ushort dist = (abs(iter->first - station.first) +
							 abs(iter->second - station.second));
		if (dist < closestdist) {
			closestdist = dist;
			closest = iter;
		}
		iter++;
	}
	return closest;
}

int zap_200(const Asteroidset& AsterCoords, const Asteroid& station) {
	// returns the coordinate of the 200th zapped asteroid in the following
	// format: (x * 100 + y)
	AngleToAsteroidSetMap* anglesMap = get_angles_map(AsterCoords, station);
	ushort zapped = 0;
	while (true) {
		// since we are iterating over 1 set of asteroids for every angle we
		// only need to find the closest one for every iteration, and can
		// safetly ignore the angle variable.
		// The map is sorted and iterating over it will start from angle = 0.0f
		// and move towards angle = 359.9f
		for (auto& [_, asteroids] : *anglesMap) {
			// asteroids here all share the same angle
			const Asteroid closest = *find_closest_asteroid(
				asteroids.begin(), asteroids.end(), station);
			if (zapped == 199) {
				delete anglesMap;
				return closest.first * 100 + closest.second;
			}
			asteroids.erase(closest);
			zapped++;
		}
	}
	throw std::logic_error("unreachable code");
}

int main(void) {
	const time_t start = clock();
	Asteroidset AsterCoords;
	std::string line;
	for (ushort y = 0; std::getline(std::cin, line); y++)
		for (ushort x = 0; x < line.size(); x++)
			if (line[x] == '#')
				AsterCoords.insert(Asteroid(x, y));
	const BestStationInfo bestStation = find_best_asteroid(AsterCoords);
#if PART1
	std::cout << "p1: " << bestStation.second << '\n';
#endif // PART1
#if PART2
	std::cout << "p2: " << zap_200(AsterCoords, bestStation.first) << '\n';
#endif // PART2
	const time_t end = clock();
	std::cout << "time: " << difftime(end, start) / CLOCKS_PER_SEC << "s\n";
	return EXIT_SUCCESS;
}
