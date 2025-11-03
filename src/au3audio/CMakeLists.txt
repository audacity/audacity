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
    ${CMAKE_CURRENT_LIST_DIR}/internal/defaultplaybackpolicy.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/defaultplaybackpolicy.h
    )

# AU3
include(${CMAKE_CURRENT_LIST_DIR}/../au3wrap/au3defs.cmake)

set(MODULE_INCLUDE ${AU3_INCLUDE})
set(MODULE_DEF ${AU3_DEF})
set(MODULE_LINK au3wrap)

setup_module()
