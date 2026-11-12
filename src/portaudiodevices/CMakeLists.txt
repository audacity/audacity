#
# Audacity: A Digital Audio Editor
#

declare_module(portaudiodevices)

set(MODULE_SRC
    ${CMAKE_CURRENT_LIST_DIR}/portaudiodevicesmodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/portaudiodevicesmodule.h
    ${CMAKE_CURRENT_LIST_DIR}/iportaudiodeviceaccess.h

    ${CMAKE_CURRENT_LIST_DIR}/internal/portaudiodevices.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/portaudiodevices.h
)

set(MODULE_LINK
    audio
    au3wrap
    au3-audio-devices
)

setup_module()
