#include <iostream>
#include <string>
#include <unordered_set>
#define PART1 1
#define PART2 1
int main(void) {
	int answer;
	std::string calcstring;
	std::string nextcalc;
	std::string::iterator iter;

	for (std::string line; std::getline(std::cin, line);) {
		calcstring += line;
	}
	calcstring += "+"; // used as a delimiter
#if PART1
	answer = 0;
	nextcalc = "";
	for (iter = calcstring.begin(); iter != calcstring.end(); ++iter) {
		if (*iter >= '0' && *iter <= '9')
			nextcalc += *iter;
		else { // start of next number / delimiter
			if (!nextcalc.empty()) {
				answer += std::stoi(nextcalc);
			}
			nextcalc = *iter; // plus or minus sign
		}
	}
	std::cout << "p1: " << answer << std::endl;
#endif // PART1
#if PART2
	answer = 0;
	nextcalc = "";
	std::unordered_set<int> answerset;
	bool running = true;
	while (running) {
		for (iter = calcstring.begin(); iter != calcstring.end(); ++iter) {
			if (*iter >= '0' && *iter <= '9')
				nextcalc += *iter;
			else { // start of next number / delimiter
				if (!nextcalc.empty()) {
					answer += std::stoi(nextcalc);
					if (!answerset.insert(answer).second) {
						std::cout << "p2: " << answer << std::endl;
						running = false;
						break;
					}
				}
				nextcalc = *iter; // plus or minus sign
			}
		}
		nextcalc = ""; // get rid of delimiter
	}
#endif // PART2
	return EXIT_SUCCESS;
}
