#include "input_big.h"

#include <algorithm>
#include <ranges>
#include <unordered_map>

#include <iostream>

namespace rng = std::ranges;

int delta(const int x, const int y) {
    return x < y
           ? 1
           : x > y
             ? -1
             : 0;
}

int main() {
    std::unordered_map<Point, size_t, PointHash> most_points;

    for (const auto line : LINES) {
        const int dx = delta(line.a.x, line.b.x);
        const int dy = delta(line.a.y, line.b.y);
        auto[x, y] = line.a;
        for (; Point{x, y} != line.b; x += dx, y += dy) {
            ++most_points[{x, y}];
        }
        ++most_points[{x, y}];
    }

    const auto points_count = rng::count_if(most_points | rng::views::values, [](auto count) { return count >= 2; });
    std::cout << points_count << std::endl;
}
