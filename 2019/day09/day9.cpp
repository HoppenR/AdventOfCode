#include "../common/IntCode.hpp"
#include <time.h>
#define PART1 1
#define PART2 1

long run_with_input(const std::vector<long>& prgState, int input) {
	IntCode BOOSTComputer(prgState, 4096);
	BOOSTComputer.inputValues.push_back(input);
	BOOSTComputer.run_program();
	return BOOSTComputer.message;
}

int main(void) {
	const time_t start = clock();
	std::vector<long> prgState;
	for (std::string opcode_str; getline(std::cin, opcode_str, ',');)
		prgState.push_back(std::stol(opcode_str));
#if PART1
	std::cout << "p1: " << run_with_input(prgState, 1) << '\n';
#endif // PART1
#if PART2
	std::cout << "p2: " << run_with_input(prgState, 2) << '\n';
#endif // PART2
	const time_t end = clock();
	std::cout << "time: " << difftime(end, start) / CLOCKS_PER_SEC << "s\n";
	return EXIT_SUCCESS;
}
