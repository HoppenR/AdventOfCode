#include "../common/IntCode.hpp"
#include <algorithm>
#include <set>
#include <time.h>
#define PART1 1
#define PART2 1

long amp_signal(const std::vector<long>& prgState,
				std::vector<int> phaseSettings, int mode) {
	std::set<long> outputSignals;
	long maxsignal = 0;
	do {
		std::vector<IntCode> Amplifiers(5, IntCode(prgState));
		for (ulong i = 0; i < Amplifiers.size(); i++) {
			Amplifiers[i].inputValues.push_back(phaseSettings[i]);
		}
		long in_output = 0;
		do {
			for (IntCode& Amp : Amplifiers) {
				Amp.inputValues.push_back(in_output);
				Amp.run_program();
				in_output = Amp.message;
			}
		} while (mode == 2 && !Amplifiers[0].halted);
		maxsignal = std::max(maxsignal, in_output);
	} while (std::next_permutation(phaseSettings.begin(), phaseSettings.end()));
	return maxsignal;
}

int main(void) {
	const time_t start = clock();
	std::vector<long> prgState;
	for (std::string opcode_str; getline(std::cin, opcode_str, ',');)
		prgState.push_back(std::stol(opcode_str));
#if PART1
	std::cout << "p1: " << amp_signal(prgState, { 0, 1, 2, 3, 4 }, 1) << '\n';
#endif // PART1
#if PART2
	std::cout << "p2: " << amp_signal(prgState, { 5, 6, 7, 8, 9 }, 2) << '\n';
#endif // PART2
	const time_t end = clock();
	std::cout << "time: " << difftime(end, start) / CLOCKS_PER_SEC << "s\n";
	return EXIT_SUCCESS;
}
