#include "../common/IntCode.hpp"
#include <algorithm>
#include <array>
#include <fstream>
#include <ncurses.h>
#define PART1 1
#define PART2 1

// NOTE: If your board is larger than 50 x 30 you need to change the numbers
//       below
#define MAXWIDTH 50
#define MAXHEIGHT 30

using tiles_t = std::array<std::array<size_t, MAXWIDTH>, MAXHEIGHT>;
static tiles_t globalTiles{ {} };

void draw_board(int score) {
	const size_t maxheight = globalTiles.size();
	const size_t maxwidth = globalTiles.at(0).size();
	const std::array<chtype, 5> sprites = {
		' ', ACS_BOARD, ACS_DIAMOND, ACS_HLINE, 'O',
	};
	for (size_t y = 0; y < maxheight; y++) {
		for (size_t x = 0; x < maxwidth; x++) {
			mvaddch(y, x, sprites.at(globalTiles.at(y).at(x)));
		}
	}
	mvprintw(maxheight, 0, "Score:%6.6d", score);
	refresh();
	// To better display the board we can nap for a few milliseconds after
	// printing Since this is called so often only 5 ms should suffice
	napms(5);
}

void iterate(IntCode& ArcadeCabinet, int& score) {
	ArcadeCabinet.run_program();
	if (ArcadeCabinet.halted)
		return;
	const long x = ArcadeCabinet.message;
	ArcadeCabinet.run_program();
	const long y = ArcadeCabinet.message;
	ArcadeCabinet.run_program();
	if (x == -1 && y == 0) {
		score = ArcadeCabinet.message;
	} else {
		globalTiles.at(unsigned(y)).at(unsigned(x)) =
			static_cast<size_t>(ArcadeCabinet.message);
	}
}

long calculate_paddle_movement() {
	int ballx = 0;
	int paddx = 0;
	for (const auto& row : globalTiles) {
		for (size_t i = 0; i < row.size(); i++) {
			if (row[i] == 3) {
				ballx = i;
			} else if (row[i] == 4) {
				paddx = i;
			}
		}
	}
	if (paddx > ballx) {
		return 1;
	} else if (paddx < ballx) {
		return -1;
	} else {
		return 0;
	}
}

int run_ai(const std::vector<long>& prgState, bool display) {
	if (display)
		initscr();
	IntCode ArcadeCabinet(prgState, 4096);
	ArcadeCabinet.prgState[0] = 2;
	ArcadeCabinet.set_input_function(calculate_paddle_movement);
	int score = 0;
	while (!ArcadeCabinet.halted) {
		iterate(ArcadeCabinet, score);
		if (display) {
			draw_board(score);
		}
	}
	if (display)
		endwin();
	return score;
}

int run_input_file(const std::vector<long>& prgState, bool display) {
	// Much faster but only works with my input
	if (display)
		initscr();
	IntCode ArcadeCabinet(prgState, 4096);
	ArcadeCabinet.prgState[0] = 2;
	std::ifstream recordedinputs("recordedinputs.txt");
	for (std::string directionInput;
		 getline(recordedinputs, directionInput, ',');) {
		ArcadeCabinet.inputValues.push_back(std::stol(directionInput));
	}
	recordedinputs.close();
	int score = 0;
	while (!ArcadeCabinet.halted) {
		iterate(ArcadeCabinet, score);
		if (display) {
			draw_board(score);
		}
	}
	if (display)
		endwin();
	return score;
}

long get_input() {
	const int c = getch();
	switch (c) {
	case KEY_LEFT:
		return -1;
	case KEY_RIGHT:
		return 1;
	default:
		return 0;
	}
}

int run_manually(const std::vector<long>& prgState) {
	initscr();
	cbreak();
	curs_set(0);
	keypad(stdscr, true);
	timeout(500);
	IntCode ArcadeCabinet(prgState, 4096);
	ArcadeCabinet.prgState[0] = 2;
	ArcadeCabinet.set_input_function(get_input);
	int score = 0;
	while (!ArcadeCabinet.halted) {
		iterate(ArcadeCabinet, score);
		draw_board(score);
	}
	endwin();
	return score;
}

int count_tiles(const std::vector<long>& prgState) {
	tiles_t localTiles{ {} };
	IntCode ArcadeCabinet(prgState, 4096);
	size_t x = 0;
	size_t y = 0;
	while (!ArcadeCabinet.halted) {
		ArcadeCabinet.run_program();
		if (ArcadeCabinet.halted)
			break;
		x = static_cast<size_t>(ArcadeCabinet.message);
		ArcadeCabinet.run_program();
		y = static_cast<size_t>(ArcadeCabinet.message);
		ArcadeCabinet.run_program();
		localTiles.at(y).at(x) = static_cast<size_t>(ArcadeCabinet.message);
	}
	int count = 0;
	for (tiles_t::iterator iter = localTiles.begin(); iter != localTiles.end();
		 iter++) {
		count += std::count(iter->begin(), iter->end(), 2);
	}
	return count;
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
	std::cout << "p1: " << count_tiles(prgState) << '\n';
#endif // PART1
#if PART2
	if (argc > 1 && argv[1][0] == 'm') {
		std::cout << "p2:\n" << run_manually(prgState) << '\n';
	} else {
		enum DisplayBoardModes { instant = 0, visible = 1 };
		std::cout << "p2: " << run_ai(prgState, instant) << '\n';
		// std::cout << "p2:\n" << run_ai (prgState, visible) << '\n';
		// std::cout << "p2: " << run_input_file(prgState, instant) << '\n';
		// std::cout << "p2:\n" << run_input_file(prgState, visible) << '\n';
	}
#endif // PART2
	const time_t end = clock();
	std::cout << "time: " << difftime(end, start) / CLOCKS_PER_SEC << "s\n";
	return EXIT_SUCCESS;
}
