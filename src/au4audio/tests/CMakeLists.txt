#
# Audacity: A Digital Audio Editor
#

set(MODULE_TEST au4audio_tests)

set(MODULE_TEST_SRC
    ${CMAKE_CURRENT_LIST_DIR}/environment.cpp

    ${CMAKE_CURRENT_LIST_DIR}/playheadmapper_tests.cpp
    )

set(MODULE_TEST_LINK
    au4audio
    au3wrap
    )

set(MODULE_TEST_DATA_ROOT ${CMAKE_CURRENT_LIST_DIR})

include(SetupGTest)
