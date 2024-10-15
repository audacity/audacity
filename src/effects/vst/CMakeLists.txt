#
# Audacity: A Digital Audio Editor
#
declare_module(effects_vst)

set(MODULE_SRC
    ${CMAKE_CURRENT_LIST_DIR}/vsteffectsmodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/vsteffectsmodule.h
    ${CMAKE_CURRENT_LIST_DIR}/ivsteffectsrepository.h

    ${CMAKE_CURRENT_LIST_DIR}/internal/vsteffectsrepository.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/vsteffectsrepository.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/vst3pluginsscanner.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/vst3pluginsscanner.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/vst3pluginsmetareader.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/vst3pluginsmetareader.h
    )

# AU3
include(${CMAKE_CURRENT_LIST_DIR}/../../au3wrap/au3defs.cmake)

set(MODULE_INCLUDE ${AU3_INCLUDE})
set(MODULE_DEF ${AU3_DEF})

set(MODULE_LINK au3wrap)

setup_module()
