#
# Audacity: A Digital Audio Editor
#

set(MODULE_TEST au3audio_tests)

set(MODULE_TEST_SRC
    ${CMAKE_CURRENT_LIST_DIR}/au3audiodrivercontroller_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/mocks/audioenginemock.h
    )

set(MODULE_TEST_LINK
    au3audio
    au3wrap
    au3-channel
    au3-exceptions
    au3-files
    au3-registries
    au3-strings
    au3-track
    au3-utility
    au3-xml
    )

set(MODULE_TEST_DATA_ROOT ${CMAKE_CURRENT_LIST_DIR})

include(SetupGTest)
