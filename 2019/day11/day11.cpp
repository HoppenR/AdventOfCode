#include "../common/IntCode.hpp"
#include <array>
#include <map>
#include <time.h>
#define PART1 1
#define PART2 1

using pos_t = std::pair<short, short>;
using hull_t = std::map<pos_t, short>;
static const std::array<pos_t, 4> movement = {
	// Represents the changes in X, Y for
	// 0 = North, 1 = East, 2 = South, 3 = West
	pos_t(0, -1),
	pos_t(1, 0),
	pos_t(0, 1),
	pos_t(-1, 0),
};

static void operator+=(pos_t& lhs, const pos_t& rhs) {
	lhs.first += rhs.first;
	lhs.second += rhs.second;
}

void paint(hull_t& hull, const std::vector<long>& prgState,
		   short starting_tile) {
	IntCode EHPR(prgState, 4096);
	pos_t pos = { 0, 0 };
	short direction = 0; // North
	EHPR.inputValues.push_back(starting_tile);
	while (true) {
		EHPR.run_program();
		// run_program() will either set message to color, or set the halt flag
		if (EHPR.halted)
			break;
		hull[pos] = EHPR.message;
		EHPR.run_program();
		// run_program() will set message to the direction to turn
		// +1 = right, -1 = left
		direction += EHPR.message ? +1 : -1;
		direction = (direction + 4) % 4;
		pos += movement.at(static_cast<size_t>(direction));
		if (hull.find(pos) != hull.end()) {
			EHPR.inputValues.push_back(hull.at(pos));
		} else {
			EHPR.inputValues.push_back(0); // Black is the default color
		}
	}
}

std::string paint_hull(const std::vector<long>& prgState) {
	hull_t hull;
	paint(hull, prgState, 1);
	std::string ret;
	for (int y = 0; y < 6; y++) {
		for (int x = 1; x < 40; x++) {
			const pos_t pos = { x, y };
			if (hull.find(pos) != hull.end()) {
				ret += hull.at(pos) ? '#' : ' ';
			}
		}
		ret += '\n';
	}
	return ret;
}

int main(void) {
	const time_t start = clock();
	std::vector<long> prgState;
	for (std::string opcode_str; getline(std::cin, opcode_str, ',');)
		prgState.push_back(std::stol(opcode_str));
#if PART1
	hull_t hull;
	paint(hull, prgState, 0);
	std::cout << "p1: " << hull.size() << '\n';
#endif // PART1
#if PART2
	std::cout << "p2:\n" << paint_hull(prgState) << std::flush;
#endif // PART2
	const time_t end = clock();
	std::cout << "time: " << difftime(end, start) / CLOCKS_PER_SEC << "s\n";
	return EXIT_SUCCESS;
}
