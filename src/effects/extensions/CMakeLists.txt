#
# Audacity: A Digital Audio Editor
#

declare_module(effects_extensions)

set(MODULE_SRC
    ${CMAKE_CURRENT_LIST_DIR}/extensioneffectsmodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/extensioneffectsmodule.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/extensioneffect.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/extensioneffect.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/extensioneffectruntime.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/extensioneffectruntime.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/extensioneffectrunner.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/extensioneffectrunner.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/extensioneffecttypes.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/extensiontypes.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/extensioneffectloader.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/extensioneffectloader.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/extensioneffectsmetareader.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/extensioneffectsmetareader.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/extensioneffectsrepository.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/extensioneffectsrepository.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/extensioneffectsscanner.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/extensioneffectsscanner.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/extensionparameterextractor.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/extensionparameterextractor.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/extensionviewlauncher.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/extensionviewlauncher.h
)

set(MODULE_LINK
        au3wrap
        au3-audio-graph
        au3-channel
        au3-exceptions
        au3-files
        au3-math
        au3-mixer
        au3-playable-track
        au3-preferences
        au3-registries
        au3-sample-track
        au3-stretching-sequence
        au3-time-frequency-selection
        au3-xml
        trackedit
        muse::extensions
        au3-command-parameters
        au3-components
        au3-effects
        au3-label-track
        au3-project
        au3-strings
        au3-track
        au3-wave-track
)
set(MODULE_USE_UNITY OFF)

setup_module()
