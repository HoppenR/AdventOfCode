#include "../common/IntCode.hpp"
#include <time.h>
#define PART1 1
#define PART2 1

int run_with_changes(const std::vector<long>& prgState, int noun, int verb) {
	IntCode Computer(prgState);
	Computer.prgState[1] = noun;
	Computer.prgState[2] = verb;
	Computer.run_program();
	return Computer.prgState[0];
}

int run_with_tweakable_changes(const std::vector<long>& prgState) {
	for (int noun = 0; noun <= 99; noun++)
		for (int verb = 0; verb <= 99; verb++)
			if (run_with_changes(prgState, noun, verb) == 19690720)
				return noun * 100 + verb;
	throw std::logic_error("Unreachable");
}

int main(void) {
	const time_t start = clock();
	std::vector<long> prgState;
	for (std::string opcode_str; getline(std::cin, opcode_str, ',');)
		prgState.push_back(std::stol(opcode_str));
#if PART1
	std::cout << "p1: " << run_with_changes(prgState, 12, 2) << '\n';
#endif // PART1
#if PART2
	std::cout << "p2: " << run_with_tweakable_changes(prgState) << '\n';
#endif // PART2
	const time_t end = clock();
	std::cout << "time: " << difftime(end, start) / CLOCKS_PER_SEC << "s\n";
	return EXIT_SUCCESS;
}
