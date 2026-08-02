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

include(${CMAKE_CURRENT_LIST_DIR}/../../au3wrap/au3wrapDefs.cmake)

set(MODULE_INCLUDE ${AU3_INCLUDE})
set(MODULE_DEF ${AU3_DEF})
set(MODULE_LINK au3wrap trackedit muse::extensions)
set(MODULE_USE_UNITY OFF)

setup_module()
