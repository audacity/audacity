#
# Audacity: A Digital Audio Editor
#

declare_module(au3audio)

set(MODULE_QML_IMPORT ${CMAKE_CURRENT_LIST_DIR}/qml)

set(MODULE_SRC
    #public
    ${CMAKE_CURRENT_LIST_DIR}/au3audiomodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/au3audiomodule.h

    # AU3
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3audioengine.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3audioengine.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3audioiolistener.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3audiometer.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3audiometer.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/defaultplaybackpolicy.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/defaultplaybackpolicy.h
    )

set(MODULE_LINK
        audio
        portaudiodevices
        au3-audio-io
        au3wrap
        au3-audio-devices
        au3-math
        au3-project-rate
        au3-time-frequency-selection
        au3-track
        au3-utility
)

setup_module()

if (AU_BUILD_AU3AUDIO_TESTS)
    add_subdirectory(tests)
endif()
