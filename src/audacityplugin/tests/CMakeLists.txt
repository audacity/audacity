# SPDX-License-Identifier: GPL-3.0-only

set(MODULE_TEST audacityplugin_tests)

set(MODULE_TEST_SRC
    ${CMAKE_CURRENT_LIST_DIR}/abi_layout_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/manifestreader_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/audacitypluginhost_tests.cpp
)

set(MODULE_TEST_INCLUDE
    ${CMAKE_CURRENT_LIST_DIR}/..
    ${PROJECT_SOURCE_DIR}/sdk/audacityplugin/v0
)

set(MODULE_TEST_LINK
    audacityplugin
)

set(MODULE_TEST_DATA_ROOT ${CMAKE_CURRENT_LIST_DIR})
include(SetupGTest)

add_library(audacityplugin_abi_layout_c OBJECT
    ${CMAKE_CURRENT_LIST_DIR}/abi_layout_c.c)
target_compile_features(audacityplugin_abi_layout_c PRIVATE c_std_11)
target_include_directories(audacityplugin_abi_layout_c PRIVATE
    ${PROJECT_SOURCE_DIR}/sdk/audacityplugin/v0)
set_target_properties(audacityplugin_abi_layout_c PROPERTIES
    C_STANDARD 11
    C_STANDARD_REQUIRED YES
    C_EXTENSIONS NO)
add_dependencies(${MODULE_TEST} audacityplugin_abi_layout_c)

add_library(audacityplugin_example_c OBJECT
    ${PROJECT_SOURCE_DIR}/sdk/audacityplugin/v0/examples/example.c)
target_compile_features(audacityplugin_example_c PRIVATE c_std_11)
target_include_directories(audacityplugin_example_c PRIVATE
    ${PROJECT_SOURCE_DIR}/sdk/audacityplugin/v0)
set_target_properties(audacityplugin_example_c PROPERTIES
    C_STANDARD 11
    C_STANDARD_REQUIRED YES
    C_EXTENSIONS NO)

add_library(audacityplugin_example_cpp OBJECT
    ${PROJECT_SOURCE_DIR}/sdk/audacityplugin/v0/examples/example.cpp)
target_compile_features(audacityplugin_example_cpp PRIVATE cxx_std_17)
target_include_directories(audacityplugin_example_cpp PRIVATE
    ${PROJECT_SOURCE_DIR}/sdk/audacityplugin/v0)
set_target_properties(audacityplugin_example_cpp PROPERTIES
    CXX_STANDARD 17
    CXX_STANDARD_REQUIRED YES
    CXX_EXTENSIONS NO)

add_dependencies(${MODULE_TEST}
    audacityplugin_example_c
    audacityplugin_example_cpp)
