#
# Audacity: A Digital Audio Editor
#

set(MODULE_TEST effects_builtin_tests)

set(MODULE_TEST_SRC
    ${CMAKE_CURRENT_LIST_DIR}/environment.cpp
    ${CMAKE_CURRENT_LIST_DIR}/DynamicRangeProcessorHistory_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/settingaccess_tests.cpp
    )

set(MODULE_TEST_INCLUDE
    ${CMAKE_CURRENT_LIST_DIR}/..
    )

set(MODULE_TEST_LINK
    au3wrap
    au3-components
    au3-dynamic-range-processor
    )

set(MODULE_TEST_DATA_ROOT ${CMAKE_CURRENT_LIST_DIR})

include(SetupGTest)
