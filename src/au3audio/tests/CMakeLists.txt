#
# Audacity: A Digital Audio Editor
#

set(MODULE_TEST au3audio_tests)

set(MODULE_TEST_SRC
    ${CMAKE_CURRENT_LIST_DIR}/environment.cpp
    ${CMAKE_CURRENT_LIST_DIR}/au3audiodevicesprovider_tests.cpp

    ${CMAKE_CURRENT_LIST_DIR}/mocks/audioenginemock.h
    )

set(MODULE_TEST_LINK
    au3audio
    wxBase
    )

set(MODULE_TEST_DATA_ROOT ${CMAKE_CURRENT_LIST_DIR})

include(SetupGTest)
