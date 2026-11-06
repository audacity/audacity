#
# Audacity: A Digital Audio Editor
#

set(MODULE_TEST au3audio_tests)

set(MODULE_TEST_SRC
    ${CMAKE_CURRENT_LIST_DIR}/environment.cpp

    ${CMAKE_CURRENT_LIST_DIR}/audiodrivercontroller_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/audioengineswitcher_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/au3audioio_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/mocks/audioenginemock.h
    )

if (AU_USE_JUCE)
    set(MODULE_TEST_SRC ${MODULE_TEST_SRC}
        ${CMAKE_CURRENT_LIST_DIR}/captureinput_tests.cpp
        ${CMAKE_CURRENT_LIST_DIR}/chainnodes_tests.cpp
        ${CMAKE_CURRENT_LIST_DIR}/juceaudiodevices_tests.cpp
        ${CMAKE_CURRENT_LIST_DIR}/meters_tests.cpp
        ${CMAKE_CURRENT_LIST_DIR}/playheadmapper_tests.cpp
    )
endif()
if (AU_USE_TRACKTION)
    set(MODULE_TEST_SRC ${MODULE_TEST_SRC}
        ${CMAKE_CURRENT_LIST_DIR}/tracktionnodes_tests.cpp
    )
endif()

set(MODULE_TEST_LINK
    au3audio
    audio
    au3wrap
    au3-audio-io
    portaudio::portaudio
    )

if (AU_USE_TRACKTION)
    set(MODULE_TEST_LINK ${MODULE_TEST_LINK} tracktion_graph)
    # tracktion_graph's headers include JUCE's
    set_source_files_properties(${CMAKE_CURRENT_LIST_DIR}/tracktionnodes_tests.cpp PROPERTIES SKIP_UNITY_BUILD_INCLUSION ON)
endif()

set(MODULE_TEST_DATA_ROOT ${CMAKE_CURRENT_LIST_DIR})

include(SetupGTest)
