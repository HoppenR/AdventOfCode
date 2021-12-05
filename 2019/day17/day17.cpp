#include "../common/IntCode.hpp"
#include <time.h>
#define PART1 1
#define PART2 0

size_t run(const std::vector<long>& prgState) {
	IntCode IntCodeComp(prgState, 4096);
	std::vector<std::vector<char>> scaffoldView;
	bool iterate = true;
	while (iterate) {
		std::vector<char> viewLine;
		do {
			IntCodeComp.run_program();
			viewLine.push_back(IntCodeComp.message);
		} while (IntCodeComp.message != 10 && !IntCodeComp.halted);
		if (!IntCodeComp.halted) {
			scaffoldView.push_back(viewLine);
		} else {
			iterate = false;
		}
	}
	size_t sum = 0;
	for (size_t y = 1; y < scaffoldView.size(); y++) {
		for (size_t x = 1; x < scaffoldView.at(y).size(); x++) {
			if (scaffoldView.at(y).at(x) == '#' &&
				scaffoldView.at(y).at(x - 1) == '#' &&
				scaffoldView.at(y).at(x + 1) == '#' &&
				scaffoldView.at(y - 1).at(x) == '#' &&
				scaffoldView.at(y + 1).at(x) == '#') {
				sum += y * x;
			}
		}
	}
	return sum;
}

void print_scaffold(const std::vector<long>& prgState) {
	IntCode IntCodeComp(prgState, 4096);
	std::vector<std::vector<char>> scaffoldView;
	bool iterate = true;
	while (iterate) {
		std::vector<char> viewLine;
		do {
			IntCodeComp.run_program();
			viewLine.push_back(IntCodeComp.message);
		} while (IntCodeComp.message != 10 && !IntCodeComp.halted);
		if (!IntCodeComp.halted) {
			scaffoldView.push_back(viewLine);
		} else {
			iterate = false;
		}
	}
	for (const auto& l : scaffoldView) {
		for (const auto& c : l) {
			std::cout << c;
		}
	}
}

int main(void) {
	const time_t start = clock();
	std::vector<long> prgState;
	for (std::string opcode_str; getline(std::cin, opcode_str, ',');)
		prgState.push_back(std::stol(opcode_str));
#if PART1
	// print_scaffold (prgState);
	std::cout << "p1: " << run(prgState) << '\n';
#endif // PART1
#if PART2

#endif // PART2
	const time_t end = clock();
	std::cout << "time: " << difftime(end, start) / CLOCKS_PER_SEC << "s\n";
	return EXIT_SUCCESS;
}
