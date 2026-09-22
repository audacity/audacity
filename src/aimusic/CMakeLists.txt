#
# AI Music Studio integration proof of concept
#

declare_module(aimusic)

set(MODULE_SRC
    ${CMAKE_CURRENT_LIST_DIR}/aimusicmodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/aimusicmodule.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/aimusiccontroller.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/aimusiccontroller.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/aimusicuiactions.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/aimusicuiactions.h
    )

include(${CMAKE_CURRENT_LIST_DIR}/../au3wrap/au3wrapDefs.cmake)

set(MODULE_INCLUDE ${AU3_INCLUDE})
set(MODULE_DEF ${AU3_DEF})
set(MODULE_LINK Qt6::Core au3wrap)
set(MODULE_USE_UNITY OFF)

setup_module()

add_subdirectory(helper)
