#include <set>
#include <list>
#include <ranges>
#include <vector>
#include <numeric>

#include <filesystem>
#include <fstream>
#include <iostream>

namespace fs = std::filesystem;
namespace ranges = std::ranges;
namespace views = std::ranges::views;

// pairwise/adjacent/zip are coming in C++23
constexpr auto zip = [](auto& xs, auto& ys) {
    return views::iota(0uz, xs.size())
           | views::transform([&](auto i) { return std::tie(xs[i], ys[i]); });
};
constexpr auto pairwise = [](auto& xs) {
    return views::iota(0uz, xs.size() - 1)
           | views::transform([&](auto i) { return std::tie(xs[i], xs[1 + i]); });
};

struct Basin {
    static constexpr size_t EMPTY_ID = std::numeric_limits<size_t>::max();
    int height;
    size_t basin_id = EMPTY_ID;
    std::list<Basin*> neighbors;
};

int main() {
    std::ifstream input{fs::path(__FILE__).parent_path() / "09.txt"};
    std::string line;
    std::vector<std::vector<Basin>> basins;
    while (std::getline(input, line)) {
        auto row = line | views::transform([](char c) { return Basin{.height = c - '0'}; });
        basins.emplace_back(std::begin(row), std::end(row));
    }

    constexpr auto neighbors = [](auto& s1, auto& s2) {
        if (s1.height == 9 || s2.height == 9) return;
        s1.neighbors.push_back(&s2);
        s2.neighbors.push_back(&s1);
    };

    for (auto[r1, r2] : pairwise(basins))
        for (auto[s1, s2] : zip(r1, r2))
            neighbors(s1, s2);

    for (auto& row : basins)
        for (auto[s1, s2] : pairwise(row))
            neighbors(s1, s2);

    auto slopes = basins | views::join
                  | views::filter([](const auto& basin) { return basin.height != 9; });

    size_t basin_id = 0;
    for (auto& slope : slopes) {
        if (slope.basin_id != Basin::EMPTY_ID) continue;
        slope.basin_id = basin_id;
        std::list<Basin*> recursive{1, &slope};
        while (!recursive.empty()) {
            Basin& next = *recursive.front();
            recursive.pop_front();

            for (auto neighbor : next.neighbors)
                if (neighbor->basin_id == Basin::EMPTY_ID)
                    neighbor->basin_id = basin_id;

            recursive.merge(next.neighbors);
        }

        ++basin_id;
    }

    std::vector<size_t> sizes(basin_id, 0uz);
    for (auto& slope : slopes) {
        ++sizes[slope.basin_id];
    }

    std::array<size_t, 4> max_sizes;
    for (const auto new_size : sizes) {
        max_sizes[0] = new_size;
        std::partial_sort(std::begin(max_sizes), std::begin(max_sizes) + 1, std::end(max_sizes));
    }

    const auto[_u, x, y, z] = max_sizes;
    std::cout << x * y * z << std::endl;  // 949905
}
