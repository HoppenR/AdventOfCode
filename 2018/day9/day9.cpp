#include <iostream>
#include <list>
#include <string>
#include <vector>
#define PART1 1
#define PART2 1
size_t marble(const size_t& num_players, const size_t& num_marbles) {
	std::vector<size_t> playerscore(num_players);
	std::vector<size_t>::iterator pscoreiter;
	std::list<size_t> marbles;
	std::list<size_t>::iterator iter;
	marbles.push_front(0);
	iter = marbles.begin();
	for (size_t m = 1; m < num_marbles; ++m) {
		if (m % 23 == 0) {
			playerscore[m % num_players] += m;
			for (size_t i = 0; i < 7; ++i) {
				if (iter == marbles.begin()) {
					iter = marbles.end();
				}
				iter--;
			}
			playerscore[m % num_players] += *iter;
			iter = marbles.erase(iter);
		} else {
			for (size_t i = 0; i < 2; ++i) {
				if (iter == marbles.end()) {
					iter = marbles.begin();
				}
				iter++;
			}
			iter = marbles.insert(iter, m);
		}
	}
	size_t best = 0;
	for (pscoreiter = playerscore.begin(); pscoreiter != playerscore.end();
		 ++pscoreiter) {
		if (*pscoreiter > best) {
			best = *pscoreiter;
		}
	}
	return best;
}

int main(void) {
	size_t num_players = 0;
	size_t num_marbles = 0;
	for (std::string line; std::getline(std::cin, line);) {
		num_players = std::stoul(line.substr(0, line.find(" ")));
		size_t endpos = line.rfind(" ");
		size_t startpos = line.rfind(" ", endpos - 1) + 1;
		num_marbles = std::stoul(line.substr(startpos, endpos - startpos));
	}
#if PART1
	std::cout << "p1: " << marble(num_players, num_marbles) << std::endl;
#endif // PART1
#if PART2
	std::cout << "p2: " << marble(num_players, num_marbles * 100) << std::endl;
#endif // PART2
	return EXIT_SUCCESS;
}
