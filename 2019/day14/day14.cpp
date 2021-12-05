#include <iostream>
#include <map>
#include <math.h>
#include <sstream>
#include <stdexcept>
#include <string>
#include <time.h>
#include <vector>
#define PART1 1
#define PART2 1

class Chemical {
public:
	size_t qty = 0;
	std::string name;
	bool operator<(const Chemical& rhs) const {
		return this->name < rhs.name;
	}
};

using RecipeBook = std::map<Chemical, std::vector<Chemical>>;
using RecipeEntry = std::pair<Chemical, std::vector<Chemical>>;
using ChemAmount = std::map<std::string, size_t>;
using ChemEntry = std::pair<std::string, size_t>;

RecipeBook::const_iterator find_chem(RecipeBook::const_iterator iter,
									 RecipeBook::const_iterator end,
									 const std::string& key) {
	while (iter != end) {
		if (iter->first.name == key) {
			return iter;
		}
		iter++;
	}
	throw(std::logic_error("chem not found"));
}

size_t reduce_fuel_into_ore(const RecipeBook& recipes, const size_t fuel) {
	ChemAmount Requirements;
	ChemAmount LeftoverInventory;
	Requirements["FUEL"] = fuel;
	Requirements["ORE"] = 0;
	// Reduce FUEL into its components until there's only ORE left, (plus
	// leftovers in LeftoverInventory)
	while (Requirements.size() > 1) {
		// Get first entry in Requirements
		ChemAmount::iterator iter = Requirements.begin();
		if (iter->first == "ORE")
			iter++;
		// Save the data
		const std::string nameToProduce = iter->first;
		size_t qtyToProduce = iter->second;
		// Delete the entry
		Requirements.erase(iter->first);
		// Take into account any left over inventory
		const ChemAmount::iterator inventoryEntry =
			LeftoverInventory.find(nameToProduce);
		if (inventoryEntry != LeftoverInventory.end()) {
			const size_t leftoverQty = inventoryEntry->second;
			const size_t qtyFromInventory =
				(leftoverQty < qtyToProduce) ? leftoverQty : qtyToProduce;
			qtyToProduce -= qtyFromInventory;
			inventoryEntry->second -= qtyFromInventory;
			if (!inventoryEntry->second) {
				LeftoverInventory.erase(inventoryEntry);
			}
			if (!qtyToProduce) {
				continue;
			}
		}
		// Find the recipe entry for information like quantity per set and the
		// chemical inputs
		const RecipeBook::const_iterator recipesEntry =
			find_chem(recipes.begin(), recipes.end(), nameToProduce);
		const size_t qtyPerSet = recipesEntry->first.qty;
		// Ceil division: (Quantity needed / Quantity produced at once) + (1 if
		// it isn't evenly divisible, 0 if it is)
		const size_t setsRequired =
			(qtyToProduce / qtyPerSet) + (qtyToProduce % qtyPerSet ? 1 : 0);
		for (const Chemical& inputChem : recipesEntry->second) {
			if (Requirements.find(inputChem.name) != Requirements.end()) {
				Requirements.at(inputChem.name) += setsRequired * inputChem.qty;
			} else {
				Requirements.insert(
					ChemEntry(inputChem.name, setsRequired * inputChem.qty));
			}
		}
		const size_t leftoverQty = (qtyPerSet * setsRequired) - qtyToProduce;
		if (leftoverQty) {
			LeftoverInventory.insert(ChemEntry(nameToProduce, leftoverQty));
		}
	}
	return Requirements["ORE"];
}

size_t count_produced_fuel(const RecipeBook& recipes, const size_t oreLimit) {
	// Solution inspired by laetus
	size_t fuelGuess = 1;
	double oreAmount = reduce_fuel_into_ore(recipes, fuelGuess);
	bool iterate = true;
	while (oreAmount < oreLimit && iterate) {
		double orePerFuel = oreAmount / fuelGuess;
		const size_t newGuess = oreLimit / orePerFuel;
		if (newGuess > fuelGuess) {
			fuelGuess = newGuess;
			oreAmount = reduce_fuel_into_ore(recipes, fuelGuess);
		} else {
			iterate = false;
		}
	}
	return fuelGuess;
}

int main(void) {
	const time_t start = clock();
	// Maps a chemical to a vector of the chemicals it depends on
	RecipeBook recipes;
	for (std::string line; std::getline(std::cin, line);) {
		const size_t recipe_io_delim = line.find(" => ");
		// Parse the recipe properties
		const std::string outStr = line.substr(recipe_io_delim + 4);
		const size_t outSpecDelim = outStr.find(" ");
		Chemical curOutChemical;
		curOutChemical.qty = std::stoul(outStr.substr(0, outSpecDelim));
		curOutChemical.name = outStr.substr(outSpecDelim + 1);
		recipes.insert(RecipeEntry(curOutChemical, {}));
		// Parse the inputs for the recipe
		std::string inChemStr = line.substr(0, recipe_io_delim);
		while (!inChemStr.empty()) {
			const size_t insSepDelim = inChemStr.find(", ");
			const std::string curIn = inChemStr.substr(0, insSepDelim);
			const size_t chemSpecSep = curIn.find(" ");
			Chemical curInChemical;
			curInChemical.qty = std::stoul(curIn.substr(0, chemSpecSep));
			curInChemical.name = curIn.substr(chemSpecSep + 1);
			recipes.at(curOutChemical).push_back(curInChemical);
			// Remove the chemical from inChemStr
			if (insSepDelim == std::string::npos) {
				inChemStr = "";
			} else {
				inChemStr = inChemStr.substr(insSepDelim + 2);
			}
		}
	}
#if PART1
	std::cout << "p1: " << reduce_fuel_into_ore(recipes, 1) << '\n';
#endif // PART 1
#if PART2
	std::cout << "p2: " << count_produced_fuel(recipes, 1000000000000) << '\n';
#endif // PART 2
	const time_t end = clock();
	std::cout << "time: " << difftime(end, start) / CLOCKS_PER_SEC << '\n';
	return EXIT_SUCCESS;
}
