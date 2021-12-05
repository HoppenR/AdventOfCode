#include <iostream>
#include <stack>
#include <string>
#define INT_MAX 2147483647

#define PART1 1
#define PART2 1

int react_polymer(const std::string& pstring) {
	std::string::const_iterator piter;
	std::stack<char> pstack;
	for (piter = pstring.begin(); piter != pstring.end(); ++piter) {
		if (pstack.empty()) {
			pstack.push(*piter);
		} else if (abs(pstack.top() - *piter) == 32) {
			pstack.pop();
		} else {
			pstack.push(*piter);
		}
	}
	return pstack.size();
}
int main(void) {
	std::string pstring;
	for (std::string line; std::getline(std::cin, line);) {
		pstring += line;
	}
#if PART1
	std::cout << "p1: " << react_polymer(pstring) << std::endl;
#endif // PART1
#if PART2
	std::string::iterator piter;
	const std::string alph = "abcdefghjklmnopqrstuvwxyz";
	int lowestsize = INT_MAX;
	for (const char& c : alph) {
		std::string alteredpstring;
		for (piter = pstring.begin(); piter != pstring.end(); ++piter) {
			if (tolower(*piter) != c) {
				alteredpstring.push_back(*piter);
			}
		}
		int size = react_polymer(alteredpstring);
		if (size < lowestsize) {
			lowestsize = size;
		}
	}
	std::cout << "p2: " << lowestsize << std::endl;
#endif // PART2
	return EXIT_SUCCESS;
}
