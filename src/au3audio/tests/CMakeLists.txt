#
# Audacity: A Digital Audio Editor
#

set(MODULE_TEST au3audio_tests)

set(MODULE_TEST_SRC
    ${CMAKE_CURRENT_LIST_DIR}/environment.cpp

    ${CMAKE_CURRENT_LIST_DIR}/au3audiodrivercontroller_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/au3audioio_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/mocks/audioenginemock.h
    ${CMAKE_CURRENT_LIST_DIR}/mocks/audiothreadloopcontroller.h
    )

set(MODULE_TEST_LINK
    au3audio
    au3wrap
    au3-audio-io
    portaudio::portaudio
    )

set(MODULE_TEST_DATA_ROOT ${CMAKE_CURRENT_LIST_DIR})

include(SetupGTest)
