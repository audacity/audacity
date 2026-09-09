#
# Audacity: A Digital Audio Editor
#

set(MODULE_TEST effects_nyquist_tests)

set(MODULE_TEST_SRC
    ${CMAKE_CURRENT_LIST_DIR}/nyquistbase_tests.cpp
)

set(MODULE_TEST_LINK
        au3wrap
        au3-nyquist-effects
)
set(MODULE_TEST_DATA_ROOT ${CMAKE_CURRENT_LIST_DIR})

include(SetupGTest)
