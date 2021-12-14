cmake_minimum_required(VERSION 3.21)
project(advent_of_code_2021_cpp)

set(CMAKE_CXX_STANDARD 23)

add_executable(advent_of_code_2021_cpp_day_5 day_5/main.cpp day_5/input_small.h day_5/input_big.h day_5/types.h)

add_executable(advent_of_code_2021_cpp_day_6 day_6/main.cpp day_6/input_small.h day_6/input_big.h day_6/types.h)

add_executable(advent_of_code_2021_cpp_day_7 day_7/main.cpp day_7/types.h day_7/input_small.h day_7/input_big.h)

add_executable(advent_of_code_2021_cpp_day_9 day_9/main.cpp)

add_executable(advent_of_code_2021_cpp_day_12 day_12/main.cpp)
