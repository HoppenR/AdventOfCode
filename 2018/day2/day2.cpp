#include <algorithm>
#include <iostream>
#include <string>
#include <unordered_set>
#define PART1 1
#define PART2 1
int main(void) {
	std::string::iterator stringiter;
	std::unordered_set<std::string> idset;
	std::unordered_set<std::string>::iterator idsetiter;

	for (std::string line; std::getline(std::cin, line);) {
		idset.insert(line);
	}
#if PART1
	int doubles = 0;
	int triples = 0;
	for (idsetiter = idset.begin(); idsetiter != idset.end(); ++idsetiter) {
		bool doubleadded = false;
		bool tripleadded = false;
		size_t ncount = 0;
		std::string currentchar;
		std::string currentstring = *idsetiter;
		std::string lastchar;
		std::sort(currentstring.begin(), currentstring.end());
		currentstring += "+"; // delimiter
		for (stringiter = currentstring.begin();
			 stringiter != currentstring.end(); ++stringiter) {
			currentchar = *stringiter;
			if (currentchar != lastchar) {
				if (ncount == 2 && !doubleadded) {
					doubles++;
					doubleadded = true;
				}
				if (ncount == 3 && !tripleadded) {
					triples++;
					tripleadded = true;
				}
				lastchar = currentchar;
				ncount = 1;
			} else {
				++ncount;
			}
		}
	}
	std::cout << "p1: " << doubles * triples << std::endl;
#endif // PART1
#if PART2
	// STOLEN FROM DonkeyThat IN CHAT:
	const size_t idlength = 26;
	std::unordered_set<std::string> riddedstringset;
	for (size_t i = 0; i < idlength; ++i) {
		for (idsetiter = idset.begin(); idsetiter != idset.end(); ++idsetiter) {
			std::string currentstring = *idsetiter;
			currentstring.replace(i, 1, "0");
			if (!riddedstringset.insert(currentstring).second) {
				currentstring.erase(i, 1);
				std::cout << "p2: " << currentstring << std::endl;
			}
		}
	}
#endif // PART2
	return EXIT_SUCCESS;
}
