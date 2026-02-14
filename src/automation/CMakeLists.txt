#
# Audacity: A Digital Audio Editor
#

declare_module(automation)

set(MODULE_SRC
    ${CMAKE_CURRENT_LIST_DIR}/automationmodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/automationmodule.h
    ${CMAKE_CURRENT_LIST_DIR}/automationtypes.h
    ${CMAKE_CURRENT_LIST_DIR}/iclipgaininteraction.h

    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3clipgaininteraction.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3clipgaininteraction.h

    # view/ settings
    ${CMAKE_CURRENT_LIST_DIR}/view/clipenvelopemodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/clipenvelopemodel.h
)

set(MODULE_USE_UNITY OFF)
set(MODULE_LINK au3wrap)

setup_module()
