#
# Audacity: A Digital Audio Editor
#

declare_module(auaudio)

set(MODULE_SRC
    ${CMAKE_CURRENT_LIST_DIR}/auaudiomodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/auaudiomodule.h
    ${CMAKE_CURRENT_LIST_DIR}/auaudiotypes.h

    ${CMAKE_CURRENT_LIST_DIR}/internal/auqttimer.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/auqttimer.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/itimer.h
)

setup_module()
