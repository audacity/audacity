#
# Audacity: A Digital Audio Editor
#

declare_module(testutility)

set(MODULE_SRC
    #public
    ${CMAKE_CURRENT_LIST_DIR}/tracktemplatefactory.cpp
    ${CMAKE_CURRENT_LIST_DIR}/tracktemplatefactory.h
    )

# AU3
include(${CMAKE_CURRENT_LIST_DIR}/../au3wrap/au3defs.cmake)

set(MODULE_INCLUDE ${AU3_INCLUDE})
set(MODULE_DEF ${AU3_DEF})
set(MODULE_LINK au3wrap)

set(MODULE_USE_UNITY OFF)

setup_module()
