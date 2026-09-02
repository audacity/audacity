#
# Audacity: A Digital Audio Editor
#

set(MODULE_TEST record_tests)

set(MODULE_TEST_SRC
    ${CMAKE_CURRENT_LIST_DIR}/environment.cpp

    ${CMAKE_CURRENT_LIST_DIR}/../../au3audio/tests/mocks/audioenginemock.h
    ${CMAKE_CURRENT_LIST_DIR}/../../audio/tests/mocks/audiodrivercontrollermock.h
    ${CMAKE_CURRENT_LIST_DIR}/mocks/recordconfigurationmock.h
    ${CMAKE_CURRENT_LIST_DIR}/mocks/recordcontrollermock.h
    ${CMAKE_CURRENT_LIST_DIR}/mocks/recordmock.h

    ${CMAKE_CURRENT_LIST_DIR}/recordcontroller_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/au3record_tests.cpp
    )

set(MODULE_TEST_INCLUDE
    ${CMAKE_CURRENT_LIST_DIR}/..
    )

set(MODULE_TEST_LINK
    record
    au3wrap
    au3-audio-graph
    au3-channel
    au3-exceptions
    au3-files
    au3-math
    au3-mixer
    au3-playable-track
    au3-preferences
    au3-registries
    au3-sample-track
    au3-stretching-sequence
    au3-strings
    au3-track
    au3-utility
    au3-wave-track
    au3-xml

    muse::ui
    )

set(MODULE_TEST_DATA_ROOT ${CMAKE_CURRENT_LIST_DIR})

include(SetupGTest)
