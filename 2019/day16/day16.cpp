#include <array>
#include <iostream>
#include <time.h>
#include <vector>
#define PART1 1
#define PART2 0

int flawed_fourier_transform(const std::vector<int>& i_vec, size_t iterations) {
	const std::array<int, 4> basePattern = { 0, 1, 0, -1 };
	std::vector<int> oldDigits = i_vec;
	for (size_t step = 0; step < iterations; step++) {
		std::vector<int> newDigits;
		for (size_t j = 0; j < i_vec.size(); j++) {
			int sum = 0;
			for (size_t k = 0; k < i_vec.size(); k++) {
				const size_t ptrnIndex = (k + 1) / (j + 1) % basePattern.size();
				sum += oldDigits.at(k) * basePattern.at(ptrnIndex);
			}
			newDigits.push_back(abs(sum % 10));
		}
		oldDigits = newDigits;
	}
	int answer = 0;
	for (size_t i = 0; i < 8; i++) {
		answer *= 10;
		answer += oldDigits.at(i);
	}
	return answer;
}

int main(void) {
	const time_t start = clock();
	std::vector<int> i_vec;
	for (std::string line; std::getline(std::cin, line);)
		for (size_t i = 0; i < line.length(); i++)
			i_vec.push_back(line.at(i) - 48);
#if PART1
	std::cout << "p1: " << flawed_fourier_transform(i_vec, 100) << '\n';
#endif // PART 1
#if PART2

#endif // PART 2
	const time_t end = clock();
	std::cout << "time: " << difftime(end, start) / CLOCKS_PER_SEC << '\n';
	return EXIT_SUCCESS;
}
