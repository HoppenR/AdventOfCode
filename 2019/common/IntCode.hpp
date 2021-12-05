#ifndef INTCODE_HEADER
#define INTCODE_HEADER
#include <functional>
#include <iostream>
#include <math.h>
#include <stdexcept>
#include <string>
#include <vector>
class IntCode {
private:
	size_t _inputValuesPtr = 0;
	int _prgPtr = 0;
	int _relativeBase = 0;
	std::function<long(void)> _inputfunction;

public:
	bool halted = false;
	long message = 0;
	std::vector<long> inputValues;
	std::vector<long> prgState;
	IntCode(const std::vector<long>& prgStateCpy);
	IntCode(const std::vector<long>& prgStateCpy, size_t size);
	long arg(int instr, int offset);
	int store_addr(int instr, int offset);
	void set_input_function(std::function<long(void)> inputfunction);
	void run_program();
};
#endif
