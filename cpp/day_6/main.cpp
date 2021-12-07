#include "input_big.h"

#include <numeric>
#include <array>
#include <ranges>

#include <iostream>

namespace rng = std::ranges;

int main() {
    std::array<uint64_t, 9> fish{};
    for (auto f1 : get_input()) {
        ++fish[f1];
    }

    constexpr auto inc_mod_9 = [](auto& x) {
        x = (x + 1) % 9;
    };

    size_t six = 6;
    size_t eight = 8;
    for (int day : rng::views::iota(0, 256)) {
        inc_mod_9(six);
        inc_mod_9(eight);
        fish[six] += fish[eight];
    }

    const auto sum = std::reduce(std::begin(fish), std::end(fish));
    std::cout << sum << std::endl;
}
