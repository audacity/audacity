#
# Audacity: A Digital Audio Editor
#
declare_module(effects_nyquist)

set(MODULE_SRC
    ${CMAKE_CURRENT_LIST_DIR}/nyquisteffectsmodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/nyquisteffectsmodule.h
    ${CMAKE_CURRENT_LIST_DIR}/inyquisteffectsrepository.h

    ${CMAKE_CURRENT_LIST_DIR}/internal/nyquisteffectsrepository.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/nyquisteffectsrepository.h
    )

# AU3
include(${CMAKE_CURRENT_LIST_DIR}/../../au3wrap/au3defs.cmake)

set(MODULE_INCLUDE ${AU3_INCLUDE})
set(MODULE_DEF ${AU3_DEF})

set(MODULE_LINK au3wrap)

setup_module()
