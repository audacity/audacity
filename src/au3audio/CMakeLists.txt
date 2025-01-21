#
# Audacity: A Digital Audio Editor
#

declare_module(audio)

set(MODULE_QML_IMPORT ${CMAKE_CURRENT_LIST_DIR}/qml)

set(MODULE_SRC
    #public
    ${CMAKE_CURRENT_LIST_DIR}/audiomodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/audiomodule.h
    ${CMAKE_CURRENT_LIST_DIR}/audiotypes.h
    ${CMAKE_CURRENT_LIST_DIR}/iaudioengine.h

    #internals
    ${CMAKE_CURRENT_LIST_DIR}/internal/audioengine.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/audioengine.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3audioiolistener.h

    # AU3
    ${CMAKE_CURRENT_LIST_DIR}/internal/defaultplaybackpolicy.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/defaultplaybackpolicy.h

    # for muse
    ${CMAKE_CURRENT_LIST_DIR}/internal/audiothreadsecurer.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/audiothreadsecurer.h
    )

# AU3
include(${CMAKE_CURRENT_LIST_DIR}/../au3wrap/au3defs.cmake)

set(MODULE_INCLUDE ${AU3_INCLUDE})
set(MODULE_DEF ${AU3_DEF})
set(MODULE_LINK au3wrap)

setup_module()
