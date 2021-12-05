#include <array>
#include <iostream>
#include <numeric> // std::lcm requires -std=c++17
#include <stdexcept>
#include <string>
#define PART1 1
#define PART2 1

class Position3D {
public:
	int x = 0;
	int y = 0;
	int z = 0;
	Position3D operator+=(const Position3D& rhs) {
		this->x += rhs.x;
		this->y += rhs.y;
		this->z += rhs.z;
		return *this;
	}
	int abs_mult(const Position3D& rhs) const {
		const int multnd = abs(x) + abs(y) + abs(z);
		const int multer = abs(rhs.x) + abs(rhs.y) + abs(rhs.z);
		return (multnd * multer);
	}
	int get_by_key(size_t axis) const {
		switch (axis) {
		case 0:
			return this->x;
		case 1:
			return this->y;
		case 2:
			return this->z;
		default:
			throw std::logic_error("invalid axis");
		}
	}
};

class Moon {
public:
	Position3D vel;
	Position3D pos;
	void update_vel(const Position3D& opos) {
		const int dx = opos.x - pos.x;
		const int dy = opos.y - pos.y;
		const int dz = opos.z - pos.z;
		this->vel.x += dx == 0 ? 0 : dx / abs(dx);
		this->vel.y += dy == 0 ? 0 : dy / abs(dy);
		this->vel.z += dz == 0 ? 0 : dz / abs(dz);
	}
	void update_pos() {
		this->pos += vel;
	}
};

using MoonArray = std::array<Moon, 4>;
using PositionArray = std::array<Position3D, 4>;

void iterate(MoonArray& moons) {
	for (Moon& moon : moons) {
		for (const Moon& omoon : moons) {
			moon.update_vel(omoon.pos);
		}
	}
	for (Moon& moon : moons) {
		moon.update_pos();
	}
}

int calculate_energy(MoonArray moons, size_t iterations) {
	for (size_t steps = 0; steps < iterations; steps++) {
		iterate(moons);
	}
	int energy = 0;
	for (const Moon& moon : moons) {
		energy += moon.vel.abs_mult(moon.pos);
	}
	return energy;
}

bool is_first_state(const MoonArray& moons, const PositionArray& omoons,
					size_t axis) {
	for (size_t i = 0; i < moons.size(); i++)
		if (!(moons[i].pos.get_by_key(axis) == omoons[i].get_by_key(axis) &&
			  moons[i].vel.get_by_key(axis) == 0))
			return false;
	return true;
}

long compute_the_universe(MoonArray moons) {
	// Credit to anon, couldn't figure out how to best do this myself
	enum Axes { x, y, z };
	const PositionArray oldPositions = {
		moons[0].pos,
		moons[1].pos,
		moons[2].pos,
		moons[3].pos,
	};
	std::array<long, 3> axes_steps{ 0, 0, 0 };
	long steps = 0;
	while (!(axes_steps[x] & axes_steps[y] & axes_steps[z])) {
		iterate(moons);
		steps++;
		for (size_t axis = 0; axis < 3; axis++) {
			if (axes_steps.at(axis))
				continue;
			if (!is_first_state(moons, oldPositions, axis))
				continue;
			axes_steps[axis] = steps;
		}
	}
	return std::lcm(std::lcm(axes_steps[x], axes_steps[y]), axes_steps[z]);
}

int main(void) {
	const time_t start = clock();
	MoonArray moons;
	for (Moon& moon : moons) {
		std::string line;
		std::getline(std::cin, line);
		if (sscanf(line.c_str(), "<x=%d, y=%d, z=%d>", &moon.pos.x, &moon.pos.y,
				   &moon.pos.z) != 3) {
			throw std::runtime_error("Faulty input");
		}
	}
#if PART1
	std::cout << "p1: " << calculate_energy(moons, 1000) << '\n';
#endif // PART1
#if PART2
	std::cout << "p2: " << compute_the_universe(moons) << '\n';
#endif // PART2
	const time_t end = clock();
	std::cout << "time: " << difftime(end, start) / CLOCKS_PER_SEC << "s\n";
	return EXIT_SUCCESS;
}
