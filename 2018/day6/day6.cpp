#include <iostream>
#include <set>
#include <string>
#include <vector>
// All yee who enter here, heed this warning: Pepega
#define INT_MAX 2147483647
#define PART2 0
int main(void) {
	std::vector<std::pair<int, int>> coordvec;
	std::vector<std::pair<int, int>>::iterator coordveciter;
	int minx = INT_MAX;
	int miny = INT_MAX;
	int maxx = 0;
	int maxy = 0;
	// FIND MIN/MAX COORDINATES AND ADD ALL COORDS TO A VECTOR
	for (std::string line; std::getline(std::cin, line);) {
		std::pair<int, int> coord;
		coord.first = std::stoi(line.substr(0, line.find(",")));
		coord.second = std::stoi(line.substr(line.find(",") + 2));
		coordvec.push_back(coord);
		if (coord.first < minx)
			minx = coord.first;
		if (coord.second < miny)
			miny = coord.second;
		if (coord.first > maxx)
			maxx = coord.first;
		if (coord.second > maxy)
			maxy = coord.second;
	}
	// FIND INVALID LANDMARKS:
	std::set<int> invalidcoords;
	std::set<int>::iterator invalciter;
	// iterate over both y axes (left and right boundary)
	for (int y = miny; y < maxy; ++y) {
		int mindistleft = INT_MAX;
		int mindistright = INT_MAX;
		int mindistidleft;
		int mindistidright;
		for (coordveciter = coordvec.begin(); coordveciter != coordvec.end();
			 ++coordveciter) {
			int deltaxleft = coordveciter->first - minx;
			int deltaxright = maxx - coordveciter->first;
			int deltay = abs(coordveciter->second - y);
			if (deltaxleft + deltay < mindistleft) {
				mindistleft = deltaxleft + deltay;
				mindistidleft = std::distance(coordvec.begin(), coordveciter);
			}
			if (deltaxright + deltay < mindistright) {
				mindistright = deltaxright + deltay;
				mindistidright = std::distance(coordvec.begin(), coordveciter);
			}
		}
		invalidcoords.insert(mindistidleft);
		invalidcoords.insert(mindistidright);
	}
	// iterate over both x axes (bottom and top boundary)
	for (int x = minx; x < maxx; ++x) {
		int mindisttop = INT_MAX;
		int mindistbot = INT_MAX;
		int mindistidtop;
		int mindistidbot;
		for (coordveciter = coordvec.begin(); coordveciter != coordvec.end();
			 ++coordveciter) {
			int deltax = abs(coordveciter->first - x);
			int deltaytop = coordveciter->second - miny;
			int deltaybot = maxy - coordveciter->second;
			if (deltax + deltaytop < mindisttop) {
				mindisttop = deltax + deltaytop;
				mindistidtop = std::distance(coordvec.begin(), coordveciter);
			}
			if (deltax + deltaybot < mindistbot) {
				mindistbot = deltax + deltaybot;
				mindistidbot = std::distance(coordvec.begin(), coordveciter);
			}
		}
		invalidcoords.insert(mindistidtop);
		invalidcoords.insert(mindistidbot);
	}
	// CALCULATE AREA OF ALL LANDMARKS:
	std::vector<int> areas(50, 0);
	std::vector<int>::iterator areaiter;
	for (int x = minx; x < maxx; ++x) {
		for (int y = miny; y < maxy; ++y) {
			bool samedistcheck = false;
			int mindistance = INT_MAX;
			size_t mindistid;
			for (coordveciter = coordvec.begin();
				 coordveciter != coordvec.end(); ++coordveciter) {
				int deltax = abs(coordveciter->first - x);
				int deltay = abs(coordveciter->second - y);
				int distance = deltax + deltay;
				if (distance < mindistance) {
					mindistance = distance;
					mindistid =
						unsigned(std::distance(coordvec.begin(), coordveciter));
					samedistcheck = false;
				} else if (distance == mindistance) {
					samedistcheck = true;
				}
			}
			if (!samedistcheck) {
				areas.at(mindistid)++;
			}
		}
	}
	// FILTER OUT INVALID AREAS
	int largestvalidarea = 0;
	for (areaiter = areas.begin(); areaiter != areas.end(); ++areaiter) {
		int i = std::distance(areas.begin(), areaiter);
		bool valid = true;
		for (invalciter = invalidcoords.begin();
			 invalciter != invalidcoords.end(); ++invalciter) {
			if (*invalciter == i) {
				valid = false;
			}
		}
		if (valid && *areaiter > largestvalidarea) {
			largestvalidarea = *areaiter;
		}
	}
	std::cout << "p1: " << largestvalidarea << std::endl;
#if PART2

#endif // PART2
	return EXIT_SUCCESS;
}
