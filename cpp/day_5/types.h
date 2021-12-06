//
// Created by Dmytro Starosud on 05.12.2021.
//

#ifndef ADVENT_OF_CODE_2021_CPP_TYPES_H
#define ADVENT_OF_CODE_2021_CPP_TYPES_H

#include <cstddef>
#include <array>
#include <tuple>

struct Point {
    int x, y;
};

struct Line {
    Point a, b;
};

template<size_t N>
using Lines = std::array<Line, N>;

bool operator==(const Point a, const Point b) {
    return a.x == b.x && a.y == b.y;
}

bool operator!=(const Point a, const Point b) {
    return !(a == b);
}

struct PointHash {
    constexpr std::size_t operator()(const Point point) const {
        return point.x * 31 + point.y;
    }
};

#endif //ADVENT_OF_CODE_2021_CPP_TYPES_H
