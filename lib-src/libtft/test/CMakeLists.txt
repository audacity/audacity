cmake_minimum_required(VERSION 3.10)

# set the project name
project(tft_test VERSION 0.1)

# specify the C++ standard
set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED True)

# add the executable
add_executable(tft_test main.cpp)
target_link_libraries (tft_test tft)

