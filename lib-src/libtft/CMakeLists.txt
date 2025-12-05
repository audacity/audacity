cmake_minimum_required(VERSION 3.10)

# set the project name
project(test)

# specify the C++ standard
set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED True)

# add the subdirs
add_subdirectory(src)
add_subdirectory(test)
