#include <iostream>
#include <string>
#define PART2 0
int traverse(void) {
	int res = 0;
	int nc, nd;
	std::cin >> nc >> nd;
	while (nc--) {
		res += traverse();
	}
	while (nd--) {
		int metadata;
		std::cin >> metadata;
		res += metadata;
	}
	return res;
}
int main(void) {
	std::cout << "p1: " << traverse() << std::endl;
#if PART2

#endif // PART2
	return EXIT_SUCCESS;
}
