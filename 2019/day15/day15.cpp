#include "../common/IntCode.hpp"
#include <fstream>
#include <ncurses.h>
#include <time.h>
#define PART1 0
#define PART2 0

using tiles_t = std::array<std::array<size_t, 60>, 60>;

static tiles_t globalTiles = { {} };
static size_t globalx = 40;
static size_t globaly = 40;
size_t lastDirection = 0;
// XXX: I did this manually for fun

long get_input() {
	const int c = getch();
	switch (c) {
	case KEY_UP:
		lastDirection = 1;
		return 1;
	case KEY_DOWN:
		lastDirection = 2;
		return 2;
	case KEY_LEFT:
		lastDirection = 3;
		return 3;
	case KEY_RIGHT:
		lastDirection = 4;
		return 4;
	default:
		return 0;
	}
}

void draw_board() {
	const size_t maxheight = globalTiles.size();
	const size_t maxwidth = globalTiles.at(0).size();
	const std::array<chtype, 5> sprites = {
		' ', ACS_BOARD, '@', ACS_DIAMOND, ACS_LANTERN,
	};
	for (size_t x = 0; x < maxwidth; x++) {
		for (size_t y = 0; y < maxheight; y++) {
			mvaddch(y, x, sprites.at(globalTiles.at(x).at(y)));
		}
	}
	mvprintw(61, 0, "Move with arrow keys ← ↓ ↑ →");
	refresh();
}

size_t compute(const std::vector<long>& prgState) {
	initscr();
	cbreak();
	curs_set(0);
	keypad(stdscr, true);
	IntCode RepairDroid(prgState, 4096);
	RepairDroid.set_input_function(get_input);
	while (true) {
		RepairDroid.run_program();
		switch (RepairDroid.message) {
		case 0:
			switch (lastDirection) {
			case 1:
				globalTiles.at(globalx).at(globaly - 1) = 1;
				break;
			case 2:
				globalTiles.at(globalx).at(globaly + 1) = 1;
				break;
			case 3:
				globalTiles.at(globalx - 1).at(globaly) = 1;
				break;
			case 4:
				globalTiles.at(globalx + 1).at(globaly) = 1;
				break;
			}
			break;
		case 1:
			globalTiles.at(globalx).at(globaly) = 3;
			switch (lastDirection) {
			case 1:
				globaly--;
				break;
			case 2:
				globaly++;
				break;
			case 3:
				globalx--;
				break;
			case 4:
				globalx++;
				break;
			}
			globalTiles.at(globalx).at(globaly) = 2;
			break;
		case 2:
			switch (lastDirection) {
			case 1:
				globalTiles.at(globalx).at(globaly - 1) = 4;
				break;
			case 2:
				globalTiles.at(globalx).at(globaly + 1) = 4;
				break;
			case 3:
				globalTiles.at(globalx - 1).at(globaly) = 4;
				break;
			case 4:
				globalTiles.at(globalx + 1).at(globaly) = 4;
				break;
			}
			break;
		}
		draw_board();
	}
}

int main(int argc, char* argv[]) {
	const time_t start = clock();
	setlocale(LC_CTYPE, ""); // Required for ncurses block types
	std::vector<long> prgState;
	if (argc > 1 && argv[1][0] == 'm') {
		// Manual mode
		// read from file directly so that stdin can be used to control the game
		std::ifstream inputfile("input");
		for (std::string opcode_str; getline(inputfile, opcode_str, ',');)
			prgState.push_back(std::stol(opcode_str));
		inputfile.close();
	} else {
		// Automatic mode
		// read puzzle input from stdin
		for (std::string opcode_str; getline(std::cin, opcode_str, ',');)
			prgState.push_back(std::stol(opcode_str));
	}
#if PART1
	if (argc > 1 && argv[1][0] == 'm') {
		compute(prgState);
	}
#endif // PART1
#if PART2

#endif // PART2
	const time_t end = clock();
	std::cout << "time: " << difftime(end, start) / CLOCKS_PER_SEC << "s\n";
	return EXIT_SUCCESS;
}
