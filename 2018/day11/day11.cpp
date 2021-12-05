#include <array>
#include <iostream>
#include <string>
#include <tuple>
#define PART1 1
#define PART2 1
// WARNING: REALLY SLOW
// (~40 seconds mid range gen 6 intel cpu)

using squareinfo = std::tuple<int, int, int, int>;
using gridplane = std::array<std::array<int, 300>, 300>;

int get_power(int x, int y, int serial) {
	int rackid = x + 10;
	int power = rackid * y;
	power += serial;
	power *= rackid;
	power /= 100;
	power %= 10;
	power -= 5;
	return power;
}

squareinfo best_power_location(gridplane grid, size_t squaresize) {
	squareinfo bestsquare;
	for (size_t x = 0; x <= (300 - squaresize); ++x) {
		for (size_t y = 0; y <= (300 - squaresize); ++y) {
			int currentscore = 0;
			for (size_t sqx = 0; sqx < squaresize; ++sqx) {
				for (size_t sqy = 0; sqy < squaresize; ++sqy) {
					currentscore += grid[x + sqx][y + sqy];
				}
			}
			if (currentscore > std::get<3>(bestsquare)) {
				bestsquare = std::make_tuple(x, y, squaresize, currentscore);
			}
		}
	}
	return bestsquare;
}

int main(void) {
	int serial;
	std::cin >> serial;
	gridplane grid;
	for (size_t x = 0; x < 300; ++x) {
		for (size_t y = 0; y < 300; ++y) {
			grid[x][y] = get_power(x, y, serial);
		}
	}
	squareinfo currentsquare;
	// [0]=x, [1]=y, [2]=size, [3]=score
#if PART1
	currentsquare = best_power_location(grid, 3);
	std::cout << "p1: " << std::get<0>(currentsquare) << ","
			  << std::get<1>(currentsquare) << std::endl;
#endif // PART1
#if PART2
	squareinfo bestsquare{ 0, 0, 0, 0 };
	for (size_t size = 0; size < 300; ++size) {
		currentsquare = best_power_location(grid, size);
		if (std::get<3>(currentsquare) > std::get<3>(bestsquare)) {
			bestsquare = currentsquare;
		}
	}
	std::cout << "p2: " << std::get<0>(bestsquare) << ","
			  << std::get<1>(bestsquare) << "," << std::get<2>(bestsquare)
			  << std::endl;
#endif // PART2
	return EXIT_SUCCESS;
}
