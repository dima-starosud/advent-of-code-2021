#include "input_big.h"

#include <ranges>
#include <execution>
#include <algorithm>
#include <numeric>

#include <iostream>

namespace ranges = std::ranges;
namespace views = std::ranges::views;

constexpr auto sum = [](auto&& range) {
    return std::reduce(std::begin(range), std::end(range));
};

int main() {
    const auto crabs = get_input();

    const auto best_sum = std::ranges::min(
            views::iota(ranges::min(crabs), 1 + ranges::max(crabs))
            | views::transform([&](const auto pos) {
                return sum(crabs | views::transform([&](const auto crab) {
                    const auto steps = std::abs(crab - pos);
                    return (1 + steps) * steps / 2;
                }));
            })
    );

    std::cout << best_sum << std::endl;
}
