#include "input_big.h"

#include <limits>
#include <array>
#include <ranges>

#include <iostream>

namespace rng = std::ranges;

int main() {
    const auto crabs = get_input();
    const auto[min_it, max_it] = std::minmax_element(std::begin(crabs), std::end(crabs));
    const auto min_pos = *min_it;
    const auto max_pos = *max_it;

    Crab best_sum = std::numeric_limits<Crab>::max();

    for (const Crab pos : rng::views::iota(min_pos, 1 + max_pos)) {
        Crab sum = 0;
        for (const Crab crab : crabs) {
            const auto steps = std::abs(crab - pos);
            sum += (1 + steps) * steps / 2;
        }
        if (sum < best_sum) {
            best_sum = sum;
        }
    }

    std::cout << best_sum << std::endl;
}
