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

# AU3 - needed because test files include AU3 headers
include(${PROJECT_SOURCE_DIR}/src/au3wrap/au3wrapDefs.cmake)

set(MODULE_TEST_INCLUDE ${AU3_INCLUDE})
set(MODULE_TEST_DEF ${AU3_DEF})
set(MODULE_TEST_DATA_ROOT ${CMAKE_CURRENT_LIST_DIR})

include(SetupGTest)
