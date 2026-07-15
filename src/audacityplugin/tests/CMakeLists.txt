# SPDX-License-Identifier: GPL-3.0-only

add_subdirectory(test-plugin)

set(MODULE_TEST audacityplugin_tests)
set(MODULE_TEST_SRC
    ${CMAKE_CURRENT_LIST_DIR}/abi_layout_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/testplugin_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/manifestreader_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/audacitypluginhost_tests.cpp
)
set(MODULE_TEST_INCLUDE
    ${CMAKE_CURRENT_LIST_DIR}/..
    ${PROJECT_SOURCE_DIR}/sdk/audacityplugin/v0
)
set(MODULE_TEST_LINK audacityplugin)
set(MODULE_TEST_DEF
    AUP_TEST_PLUGIN_BUNDLE_PATH="${AUDACITYPLUGIN_TEST_PLUGIN_BUNDLE_DIR}"
)
set(MODULE_TEST_DATA_ROOT ${CMAKE_CURRENT_LIST_DIR})
include(SetupGTest)

add_library(audacityplugin_sdk_checks OBJECT
    ${CMAKE_CURRENT_LIST_DIR}/abi_layout_c.c
    ${PROJECT_SOURCE_DIR}/sdk/audacityplugin/v0/examples/example.c
    ${PROJECT_SOURCE_DIR}/sdk/audacityplugin/v0/examples/example.cpp
)
target_compile_features(audacityplugin_sdk_checks PRIVATE
    c_std_11
    cxx_std_17
)
target_include_directories(audacityplugin_sdk_checks PRIVATE
    ${PROJECT_SOURCE_DIR}/sdk/audacityplugin/v0
)
set_target_properties(audacityplugin_sdk_checks PROPERTIES
    AUTOMOC OFF
    AUTORCC OFF
    AUTOUIC OFF
    C_EXTENSIONS NO
    CXX_EXTENSIONS NO
)

add_dependencies(${MODULE_TEST}
    audacityplugin_test_plugin
    audacityplugin_sdk_checks
)
