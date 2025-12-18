#
# Audacity: A Digital Audio Editor
#

declare_module(audio)

set(MODULE_SRC
    ${CMAKE_CURRENT_LIST_DIR}/audiomodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/audiomodule.h
    ${CMAKE_CURRENT_LIST_DIR}/audiotypes.h
    ${CMAKE_CURRENT_LIST_DIR}/iaudioengine.h
    ${CMAKE_CURRENT_LIST_DIR}/iaudiometer.h
    ${CMAKE_CURRENT_LIST_DIR}/iaudiodevicesprovider.h

    ${CMAKE_CURRENT_LIST_DIR}/internal/audiometer.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/audiometer.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/auqttimer.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/auqttimer.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/itimer.h

    # for muse
    ${CMAKE_CURRENT_LIST_DIR}/internal/audiothreadsecurer.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/audiothreadsecurer.h
)

set(MODULE_LINK au3wrap) # For LockFreeQueue

setup_module()
