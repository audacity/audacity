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
    ${CMAKE_CURRENT_LIST_DIR}/view/clipgainmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/clipgainmodel.h
)

set(MODULE_USE_UNITY OFF)
set(MODULE_LINK
        au3wrap
        au3-audio-graph
        au3-channel
        au3-exceptions
        au3-files
        au3-label-track
        au3-math
        au3-mixer
        au3-playable-track
        au3-preferences
        au3-registries
        au3-sample-track
        au3-stretching-sequence
        au3-strings
        au3-time-frequency-selection
        au3-track
        au3-utility
        au3-wave-track
        au3-xml
)

setup_module()

if (AU_BUILD_AUTOMATION_TESTS)
    add_subdirectory(tests)
endif()
