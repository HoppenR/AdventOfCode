#include <array>
#include <climits>
#include <iostream>
#include <string>
#include <time.h>
#define PART1 1
#define PART2 1

using row_t = std::array<size_t, 25>;
using layer_t = std::array<row_t, 6>;
using picture_t = std::array<layer_t, 100>;

int find_layer_least_zeroes(const picture_t& picture) {
	int answer = 0;
	int fewestzeroes = INT_MAX;
	for (const layer_t& layer : picture) {
		std::array<int, 3> layercounts = { 0, 0, 0 };
		for (const row_t& row : layer) {
			for (size_t i : row) {
				layercounts[i]++;
			}
		}
		if (layercounts[0] < fewestzeroes) {
			fewestzeroes = layercounts[0];
			answer = layercounts[1] * layercounts[2];
		}
	}
	return answer;
}

std::string decode_picture_to_string(const picture_t& picture) {
	std::string answer;
	for (size_t r_n = 0; r_n < 6; r_n++) {
		for (size_t i_n = 0; i_n < 25; i_n++) {
			for (const layer_t& layer : picture) {
				if (layer[r_n][i_n] != 2) {
					answer += (layer[r_n][i_n] ? '#' : ' ');
					break;
				}
			}
		}
		answer += '\n';
	}
	return answer;
}

int main(void) {
	const time_t start = clock();
	picture_t picture;
	// 48 DEC = '0' CHAR
	// 49 DEC = '1' CHAR
	// 50 DEC = '2' CHAR
	for (layer_t& layer : picture)
		for (row_t& row : layer)
			for (size_t& i : row)
				i = static_cast<size_t>(getchar() - 48);
#if PART1
	std::cout << "p1: " << find_layer_least_zeroes(picture) << '\n';
#endif // PART1
#if PART2
	std::cout << "p2:\n" << decode_picture_to_string(picture) << std::flush;
#endif // PART2
	const time_t end = clock();
	std::cout << "time: " << difftime(end, start) / CLOCKS_PER_SEC << "s\n";
	return EXIT_SUCCESS;
}
