#include <iostream>
#include <string>
#include <time.h>
#include <vector>
#define PART1 1
#define PART2 1

int calculate_mass(int mass) {
	return mass / 3 - 2;
}

int main(void) {
	const time_t start = clock();
	std::vector<int> input;
	for (std::string line; std::getline(std::cin, line);)
		input.push_back(std::stoi(line));
#if PART1
	int p1ans = 0;
	for (int modulemass : input) {
		p1ans += calculate_mass(modulemass);
	}
	std::cout << "p1: " << p1ans << '\n';
#endif // PART1
#if PART2
	int p2ans = 0;
	for (int modulemass : input) {
		// calculate_mass (n) for (n < 9) returns 0 so we can ignore those
		// numbers
		while (modulemass >= 9) {
			modulemass = calculate_mass(modulemass);
			p2ans += modulemass;
		}
	}
	std::cout << "p2: " << p2ans << '\n';
#endif // PART2
	const time_t end = clock();
	std::cout << "time: " << difftime(end, start) / CLOCKS_PER_SEC << "s\n";
	return EXIT_SUCCESS;
}
