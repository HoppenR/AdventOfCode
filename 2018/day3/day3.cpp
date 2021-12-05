#include <array>
#include <iostream>
#include <string>
#include <unordered_set>
#define PART2 1
int main(void) {
	std::unordered_set<std::string> idset;
	std::unordered_set<std::string>::iterator idsetiter;
	for (std::string line; std::getline(std::cin, line);) {
		idset.insert(line);
	}
	std::array<std::array<int, 1000>, 1000> fabric;
	size_t result = 0;
	for (idsetiter = idset.begin(); idsetiter != idset.end(); ++idsetiter) {
		size_t atloc = idsetiter->find("@");
		// std::string claim = idsetiter->substr(1, atloc - 2);
		size_t colonloc = idsetiter->find(":");
		std::string coordinates =
			idsetiter->substr(atloc + 2, colonloc - (atloc + 2));
		size_t commaloc = coordinates.find(",");
		std::string geometry = idsetiter->substr(colonloc + 2);
		size_t xloc = geometry.find("x");
		size_t offsetx = std::stoul(coordinates.substr(0, commaloc));
		size_t offsety = std::stoul(coordinates.substr(commaloc + 1));
		size_t height = std::stoul(geometry.substr(xloc + 1));
		size_t width = std::stoul(geometry.substr(0, xloc));
		for (size_t x = 0; x < width; ++x) {
			for (size_t y = 0; y < height; ++y) {
				if (++fabric[offsetx + x][offsety + y] == 2) {
					result++;
				}
			}
		}
	}
	std::cout << "p1: " << result << std::endl;
#if PART2
	for (idsetiter = idset.begin(); idsetiter != idset.end(); ++idsetiter) {
		size_t atloc = idsetiter->find("@");
		std::string claim = idsetiter->substr(1, atloc - 2);
		size_t colonloc = idsetiter->find(":");
		std::string coordinates =
			idsetiter->substr(atloc + 2, colonloc - (atloc + 2));
		size_t commaloc = coordinates.find(",");
		std::string geometry = idsetiter->substr(colonloc + 2);
		size_t xloc = geometry.find("x");
		size_t offsetx = std::stoul(coordinates.substr(0, commaloc));
		size_t offsety = std::stoul(coordinates.substr(commaloc + 1));
		size_t height = std::stoul(geometry.substr(xloc + 1));
		size_t width = std::stoul(geometry.substr(0, xloc));
		bool nooverlap = true;
		for (size_t x = 0; x < width; ++x) {
			for (size_t y = 0; y < height; ++y) {
				if (fabric[offsetx + x][offsety + y] > 1)
					nooverlap = false;
			}
		}
		if (nooverlap) {
			std::cout << "p2: " << claim << std::endl;
		}
	}
#endif // PART2
	return EXIT_SUCCESS;
}
