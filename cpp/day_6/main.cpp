#include "input_big.h"

#include <numeric>
#include <array>
#include <ranges>

#include <iostream>

namespace rng = std::ranges;

int main() {
    std::array<long long int, 9> fish{};
    for (auto f1 : get_input()) {
        ++fish[f1];
    }

    for (int day : rng::views::iota(0, 256)) {
        const auto zero_fish = fish[0];
        for (size_t number = 1; number < fish.size(); ++number) {
            fish[number - 1] = fish[number];
        }
        fish[6] += zero_fish;
        fish[8] = zero_fish;
    }

    const auto sum = std::accumulate(std::begin(fish), std::end(fish), static_cast<long long int>(0));
    std::cout << sum << std::endl;
}
