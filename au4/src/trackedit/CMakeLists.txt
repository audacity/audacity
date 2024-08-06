#
# Audacity: A Digital Audio Editor
#

declare_module(trackedit)

set(MODULE_SRC
    ${CMAKE_CURRENT_LIST_DIR}/trackeditmodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/trackeditmodule.h
    ${CMAKE_CURRENT_LIST_DIR}/trackedittypes.h
    ${CMAKE_CURRENT_LIST_DIR}/itrackeditproject.h
    ${CMAKE_CURRENT_LIST_DIR}/itrackeditinteraction.h
    ${CMAKE_CURRENT_LIST_DIR}/itrackeditactionscontroller.h
    ${CMAKE_CURRENT_LIST_DIR}/iselectioncontroller.h

    ${CMAKE_CURRENT_LIST_DIR}/dom/track.h
    ${CMAKE_CURRENT_LIST_DIR}/dom/clip.cpp
    ${CMAKE_CURRENT_LIST_DIR}/dom/clip.h
    ${CMAKE_CURRENT_LIST_DIR}/dom/wave.cpp
    ${CMAKE_CURRENT_LIST_DIR}/dom/wave.h

    ${CMAKE_CURRENT_LIST_DIR}/internal/trackedituiactions.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/trackedituiactions.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/trackeditactionscontroller.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/trackeditactionscontroller.h

    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3trackeditproject.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3trackeditproject.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3selectioncontroller.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3selectioncontroller.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3interaction.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3interaction.h
    )

# AU3
# AU3
include(${CMAKE_CURRENT_LIST_DIR}/../au3wrap/au3defs.cmake)

set(MODULE_INCLUDE ${AU3_INCLUDE})
set(MODULE_DEF ${AU3_DEF})
set(MODULE_LINK au3wrap)

set(MODULE_LINK
    ${MODULE_LINK}
    muse::draw
)

setup_module()
