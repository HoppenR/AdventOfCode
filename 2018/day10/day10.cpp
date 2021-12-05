#include <iostream>
#include <string>
#include <tuple>
#include <vector>
#define INT_MAX 2147483647

#define PART1 1
#define PART2 1
#define MAX_HEIGHT 10
// Sadly, 10 height characters just happened to work for me, it might not for
// other inputs

using quadtuple = std::tuple<int, int, int, int>;

quadtuple get_extremes(const std::vector<quadtuple>& stars) {
	std::vector<quadtuple>::const_iterator stariter;
	int minx = INT_MAX;
	int miny = INT_MAX;
	int maxx = 0;
	int maxy = 0;
	for (stariter = stars.begin(); stariter != stars.end(); ++stariter) {
		int currentx = std::get<0>(*stariter);
		int currenty = std::get<1>(*stariter);
		if (currentx < minx)
			minx = currentx;
		if (currentx > maxx)
			maxx = currentx;
		if (currenty < miny)
			miny = currenty;
		if (currenty > maxy)
			maxy = currenty;
	}
	return std::make_tuple(minx, miny, maxx, maxy);
}

void print_stars(const std::vector<quadtuple>& stars, quadtuple extremecoords) {
	std::vector<quadtuple>::const_iterator stariter;
	for (int y = std::get<1>(extremecoords); y <= std::get<3>(extremecoords);
		 ++y) {
		for (int x = std::get<0>(extremecoords);
			 x <= std::get<2>(extremecoords); ++x) {
			bool exists = false;
			for (stariter = stars.begin(); stariter != stars.end();
				 ++stariter) {
				int currentx = std::get<0>(*stariter);
				int currenty = std::get<1>(*stariter);
				if (currentx == x && currenty == y) {
					exists = true;
				}
			}
			if (exists) {
				std::cout << "#";
			} else {
				std::cout << ".";
			}
		}
		std::cout << std::endl;
	}
}

void step(std::vector<quadtuple>& stars) {
	std::vector<quadtuple>::iterator stariter;
	for (stariter = stars.begin(); stariter != stars.end(); ++stariter) {
		std::get<0>(*stariter) += std::get<2>(*stariter);
		std::get<1>(*stariter) += std::get<3>(*stariter);
	}
}

int main(void) {
	std::vector<quadtuple> stars;
	for (std::string line; std::getline(std::cin, line);) {
		int x = std::stoi(line.substr(10, 6));
		int y = std::stoi(line.substr(18, 6));
		int vx = std::stoi(line.substr(36, 2));
		int vy = std::stoi(line.substr(40, 2));
		stars.push_back(std::make_tuple(x, y, vx, vy));
		// [0]=x, [1]=y, [2]=velocity x, [3]=velocity y
	}
	quadtuple extremecoords = { -100, -100, 100, 100 };
	// [0]=minx, [1]=miny, [2]=maxx, [3]=maxy
	int steps = 0;
	while ((std::get<3>(extremecoords) - std::get<1>(extremecoords)) + 1 >
		   MAX_HEIGHT) {
		step(stars);
		extremecoords = get_extremes(stars);
		steps++;
	}
#if PART1
	std::cout << "p1: " << std::endl;
	print_stars(stars, extremecoords);
#endif // PART1
#if PART2
	std::cout << "p2: " << steps << std::endl;
#endif // PART2
	return EXIT_SUCCESS;
}
