#include <algorithm>
#include <iostream>
#include <map>
#include <string>
#include <vector>
#define PART2 1
int main(void) {
	std::vector<std::string> inputvec;
	std::vector<std::string>::iterator inItr;
	std::map<size_t, std::vector<size_t>> guardmap;
	std::map<size_t, std::vector<size_t>>::iterator guardmapiter;
	for (std::string line; std::getline(std::cin, line);) {
		inputvec.push_back(line);
	}
	std::sort(inputvec.begin(), inputvec.end());
	size_t sleepstart;
	size_t waketime;
	size_t guardid;
	for (inItr = inputvec.begin(); inItr != inputvec.end(); ++inItr) {
		if (inItr->find("G") != std::string::npos) { // "Guard"
			// 26 is the first column in which guard IDs can appear
			guardid = std::stoul(inItr->substr(26, inItr->find(" ", 26) - 26));
			std::vector<size_t> timetable(59, 0);
			std::pair<size_t, std::vector<size_t>> guardinfo =
				std::make_pair(guardid, timetable);
			guardmap.insert(guardinfo);
		} else if (inItr->find("f") != std::string::npos) { // "falls asleep"
			sleepstart = std::stoul(inItr->substr(15, 17));
		} else if (inItr->find("w") != std::string::npos) { // "wakes up"
			waketime = std::stoul(inItr->substr(15, 17));
			for (size_t i = sleepstart; i < waketime; ++i) {
				guardmap.at(guardid)[i]++;
			}
		}
	}
	size_t mostsleep = 0;
	size_t targetguardid;
	std::vector<size_t> timetable(59, 0);
	for (guardmapiter = guardmap.begin(); guardmapiter != guardmap.end();
		 ++guardmapiter) {
		size_t sleepcounter = 0;
		for (size_t i = 0; i < 59; ++i) {
			sleepcounter += guardmapiter->second[i];
			timetable[i] += guardmapiter->second[i];
		}
		if (sleepcounter > mostsleep) {
			targetguardid = guardmapiter->first;
			mostsleep = sleepcounter;
		}
	}
	size_t mostsleptminute;
	size_t targettime = 0;
	for (size_t i = 0; i < 59; ++i) {
		if (timetable[i] > targettime) {
			targettime = timetable[i];
			mostsleptminute = i;
		}
	}
	std::cout << "p1: " << targetguardid * mostsleptminute << std::endl;
#if PART2
	size_t mostslepttimes = 0;
	size_t mostslepttimesguardid;
	size_t mostslepttimesminute;
	for (guardmapiter = guardmap.begin(); guardmapiter != guardmap.end();
		 ++guardmapiter) {
		for (size_t i = 0; i < 59; ++i) {
			if (guardmapiter->second[i] > mostslepttimes) {
				mostslepttimes = guardmapiter->second[i];
				mostslepttimesminute = i;
				mostslepttimesguardid = guardmapiter->first;
			}
		}
	}
	std::cout << "p2: " << mostslepttimesminute * mostslepttimesguardid
			  << std::endl;
#endif // PART2
	return EXIT_SUCCESS;
}
