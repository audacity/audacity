# SPDX-License-Identifier: GPL-3.0-or-later

set(_bundle_dir
    "${CMAKE_CURRENT_BINARY_DIR}/org.audacityteam.audacityplugin-test.audacity4-plugin")
set(AUP_TEST_PLUGIN_BINARY_NAME
    "audacityplugin-test${CMAKE_SHARED_MODULE_SUFFIX}")

file(MAKE_DIRECTORY "${_bundle_dir}")
configure_file(plugin.json.in "${_bundle_dir}/plugin.json" @ONLY)

add_library(audacityplugin_test_plugin MODULE test_plugin.c)
target_compile_features(audacityplugin_test_plugin PRIVATE c_std_11)
target_include_directories(audacityplugin_test_plugin PRIVATE
    ${PROJECT_SOURCE_DIR}/sdk/audacityplugin/v0)

set_target_properties(audacityplugin_test_plugin PROPERTIES
    AUTOMOC OFF
    AUTORCC OFF
    AUTOUIC OFF
    C_EXTENSIONS NO
    C_VISIBILITY_PRESET hidden
    PREFIX ""
    OUTPUT_NAME "audacityplugin-test"
    LIBRARY_OUTPUT_DIRECTORY "${_bundle_dir}/$<0:>"
    RUNTIME_OUTPUT_DIRECTORY "${_bundle_dir}/$<0:>"
)

set(AUDACITYPLUGIN_TEST_PLUGIN_BUNDLE_DIR "${_bundle_dir}" PARENT_SCOPE)
