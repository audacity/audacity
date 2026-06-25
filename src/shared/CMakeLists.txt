#
# Audacity: A Digital Audio Editor
#

# Cross-cutting C++ types and utilities that don't belong to any one feature
# module. Mirrors the role of muse_framework's `global` at the app level.

declare_module(shared)

set(MODULE_SRC
    ${CMAKE_CURRENT_LIST_DIR}/axis/axisscale.h
    ${CMAKE_CURRENT_LIST_DIR}/axis/axistypes.h
    ${CMAKE_CURRENT_LIST_DIR}/axis/numberscale.h
    ${CMAKE_CURRENT_LIST_DIR}/axis/axisticks.h
    ${CMAKE_CURRENT_LIST_DIR}/axis/axisticks.cpp
    ${CMAKE_CURRENT_LIST_DIR}/axis/axislabel.h
    ${CMAKE_CURRENT_LIST_DIR}/axis/axislabel.cpp

    ${CMAKE_CURRENT_LIST_DIR}/types/decibel.h
    ${CMAKE_CURRENT_LIST_DIR}/types/decibel.cpp
)

setup_module()

if (AU_BUILD_SHARED_TESTS)
    add_subdirectory(tests)
endif()
