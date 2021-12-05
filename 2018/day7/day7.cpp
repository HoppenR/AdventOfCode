#include <algorithm>
#include <iostream>
#include <map>
#include <string>
#define PART2 0
int main(void) {
	std::map<char, std::string> depMap;
	std::map<char, std::string>::iterator depMapiter;
	std::string::iterator stringiter;
	std::map<char, int> numDep;
	for (std::string line; std::getline(std::cin, line);) {
		char x = line[5];
		char y = line[36];
		depMap[x] += y;
		numDep[y] += 1;
	}
	std::string charque("");
	for (depMapiter = depMap.begin(); depMapiter != depMap.end();
		 ++depMapiter) {
		if (numDep[depMapiter->first] == 0) {
			charque += depMapiter->first;
		}
	}
	std::string answer("");
	while (!charque.empty()) {
		std::sort(charque.begin(), charque.end());
		char x = charque.front();
		charque.erase(0, 1);
		answer += x;
		for (stringiter = depMap[x].begin(); stringiter != depMap[x].end();
			 ++stringiter) {
			numDep[*stringiter] -= 1;
			if (numDep[*stringiter] == 0) {
				charque += *stringiter;
			}
		}
	}
	std::cout << "p1: " << answer << std::endl;
#if PART2

#endif // PART2
	return EXIT_SUCCESS;
}
