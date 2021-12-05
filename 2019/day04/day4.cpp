#include <algorithm>
#include <iostream>
#include <map>
#include <math.h>
#include <stdexcept>
#include <string>
#include <time.h>
#include <vector>
#define PART1 1
#define PART2 1

bool has_exactly_two(const std::vector<int>& i_vec) {
	std::map<int, int> chcounter;
	for (int i : i_vec)
		chcounter[i] += 1;
	for (const std::pair<int, int>& num_ch : chcounter)
		if (num_ch.second == 2)
			return true;
	return false;
}

int getn(int number, int n) {
	// returns the nth digit in number, counted from right hand side, 0-based
	return number / static_cast<int>(pow(10, n)) % 10;
}

bool has_six_digits(int num) {
	return floor((log10(num) + 1)) == 6.0f;
}

int count_valid_pass(int lowerbound, int upperbound, int ruleset) {
	int npasses = 0;
	for (int i = lowerbound; i < upperbound; i++) {
		/* if(!has_six_digits(i)) continue; // use if your range is outside 6
		 * digits */
		std::vector<int> i_vec = {
			getn(i, 5), getn(i, 4), getn(i, 3),
			getn(i, 2), getn(i, 1), getn(i, 0),
		};
		if (!std::is_sorted(i_vec.begin(), i_vec.end()))
			continue;
		if (ruleset == 1 &&
			!(std::adjacent_find(i_vec.begin(), i_vec.end()) != i_vec.end()))
			continue;
		if (ruleset == 2 && !(has_exactly_two(i_vec)))
			continue;
		npasses++;
	}
	return npasses;
}

int main(void) {
	const time_t start = clock();
	std::string input;
	if (!getline(std::cin, input))
		throw std::runtime_error("input is not 1 line!");
	const size_t divider = input.find("-");
	const int lowerbound = std::stoi(input.substr(0, divider));
	const int upperbound = std::stoi(input.substr(divider + 1));
	if (!has_six_digits(lowerbound))
		throw std::runtime_error("lower bound is not 6 digits!");
	if (!has_six_digits(upperbound))
		throw std::runtime_error("upper bound is not 6 digits!");
#if PART1
	std::cout << "p1: " << count_valid_pass(lowerbound, upperbound, 1) << '\n';
#endif // PART1
#if PART2
	std::cout << "p2: " << count_valid_pass(lowerbound, upperbound, 2) << '\n';
#endif // PART2
	const time_t end = clock();
	std::cout << "time: " << difftime(end, start) / CLOCKS_PER_SEC << "s\n";
	return EXIT_SUCCESS;
}
