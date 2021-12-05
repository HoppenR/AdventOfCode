#include "./IntCode.hpp"
// Lots of -Wsign-conversion errors in here but I've checked the usage (I think)

IntCode::IntCode(const std::vector<long>& prgStateCpy) {
	this->prgState = prgStateCpy;
}

IntCode::IntCode(const std::vector<long>& prgStateCpy, size_t size) {
	this->prgState = prgStateCpy;
	// Yes, yes, I know.
	this->prgState.resize(size);
}

long IntCode::arg(int instr, int offset) {
	const int mode = instr / (static_cast<int>(pow(10, offset + 1))) % 10;
	switch (mode) {
	case 0:
		return prgState.at(prgState.at(_prgPtr + offset));
	case 1:
		return prgState.at(_prgPtr + offset);
	case 2:
		return prgState.at(_relativeBase + prgState.at(_prgPtr + offset));
	default:
		throw std::runtime_error("Unimplemented parameter mode: " +
								 std::to_string(mode));
	}
}

int IntCode::store_addr(int instr, int offset) {
	const int mode = instr / (static_cast<int>(pow(10, offset + 1))) % 10;
	switch (mode) {
	case 0:
		return prgState.at(_prgPtr + offset);
	case 2:
		return _relativeBase + prgState.at(_prgPtr + offset);
	default:
		throw std::runtime_error("Unimplemented parameter mode: " +
								 std::to_string(mode));
	}
}

void IntCode::set_input_function(std::function<long(void)> inputfunction) {
	this->_inputfunction = inputfunction;
}

void IntCode::run_program() {
	while (true) {
		const int instr = prgState.at(_prgPtr);
		const int opCode = instr % 100;
		switch (opCode) {
		case 99:
			halted = true;
			return;
		case 1:
			prgState.at(store_addr(instr, 3)) = arg(instr, 1) + arg(instr, 2);
			_prgPtr += 4;
			break;
		case 2:
			prgState.at(store_addr(instr, 3)) = arg(instr, 1) * arg(instr, 2);
			_prgPtr += 4;
			break;
		case 3:
			if (_inputValuesPtr >= inputValues.size()) {
				if (_inputfunction == nullptr) {
					throw std::logic_error(
						"Not enough input provided "
						"and no input function was provided");
				}
				inputValues.push_back(_inputfunction());
			}
			prgState.at(store_addr(instr, 1)) = inputValues.at(_inputValuesPtr);
			_inputValuesPtr++;
			_prgPtr += 2;
			break;
		case 4:
			message = arg(instr, 1);
			_prgPtr += 2;
			return;
		case 5:
			_prgPtr = arg(instr, 1) ? arg(instr, 2) : (_prgPtr + 3);
			break;
		case 6:
			_prgPtr = !arg(instr, 1) ? arg(instr, 2) : (_prgPtr + 3);
			break;
		case 7:
			prgState.at(store_addr(instr, 3)) = arg(instr, 1) < arg(instr, 2);
			_prgPtr += 4;
			break;
		case 8:
			prgState.at(store_addr(instr, 3)) = arg(instr, 1) == arg(instr, 2);
			_prgPtr += 4;
			break;
		case 9:
			_relativeBase += arg(instr, 1);
			_prgPtr += 2;
			break;
		default:
			throw std::runtime_error("Unimplemented opCode: " +
									 std::to_string(opCode));
		}
	}
}
