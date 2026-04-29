#
# Audacity: A Digital Audio Editor
#

set(MODULE_TEST effects_nyquist_tests)

set(MODULE_TEST_SRC
    ${CMAKE_CURRENT_LIST_DIR}/nyquistbase_tests.cpp
)

# AU3 - needed because test files include AU3 headers
include(${PROJECT_SOURCE_DIR}/src/au3wrap/au3wrapDefs.cmake)

set(MODULE_TEST_INCLUDE ${AU3_INCLUDE})
set(MODULE_TEST_DEF ${AU3_DEF})
set(MODULE_TEST_LINK
    au3-nyquist-effects
    ${AU3_LINK}
)
set(MODULE_TEST_DATA_ROOT ${CMAKE_CURRENT_LIST_DIR})

include(SetupGTest)
