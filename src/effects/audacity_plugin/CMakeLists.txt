# SPDX-License-Identifier: GPL-3.0-only

declare_module(effects_audacity_plugin)

set(MODULE_SRC
    ${CMAKE_CURRENT_LIST_DIR}/audacityplugineffectsmodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/audacityplugineffectsmodule.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/audacityplugineffect.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/audacityplugineffect.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/audacityplugineffectoffline.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/audacitypluginofflinehost.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/audacitypluginofflinehost.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/audacityplugineffectloader.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/audacityplugineffectloader.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/audacitypluginids.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/audacitypluginids.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/audacitypluginparameterextractorservice.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/audacitypluginparameterextractorservice.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/audacitypluginmetareader.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/audacitypluginmetareader.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/audacitypluginscanner.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/audacitypluginscanner.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/audacitypluginviewlauncher.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/audacitypluginviewlauncher.h
)

include(${CMAKE_CURRENT_LIST_DIR}/../../au3wrap/au3wrapDefs.cmake)

set(MODULE_INCLUDE ${AU3_INCLUDE})
set(MODULE_DEF ${AU3_DEF})
set(MODULE_LINK effects_base audacityplugin au3wrap)

set(MODULE_USE_UNITY OFF)
setup_module()

if (AU_BUILD_EFFECTS_TESTS)
    add_subdirectory(tests)
endif()
