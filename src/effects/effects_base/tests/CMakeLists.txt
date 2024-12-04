#
# Audacity: A Digital Audio Editor
#

set(MODULE_TEST effects_base_tests)

set(MODULE_TEST_SRC
    ${CMAKE_CURRENT_LIST_DIR}/environment.cpp
    ${CMAKE_CURRENT_LIST_DIR}/config_tests.cpp
)

set(MODULE_TEST_LINK
    effects_base
)

set(MODULE_TEST_DATA_ROOT ${CMAKE_CURRENT_LIST_DIR})

include(SetupGTest)
