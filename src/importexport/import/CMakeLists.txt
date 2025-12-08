#
# Audacity: A Digital Audio Editor
#

declare_module(importer)

set(MODULE_SRC
    # public
    ${CMAKE_CURRENT_LIST_DIR}/importermodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/importermodule.h
    ${CMAKE_CURRENT_LIST_DIR}/iimporter.h
    ${CMAKE_CURRENT_LIST_DIR}/ilabelsimporter.h
    ${CMAKE_CURRENT_LIST_DIR}/iimportconfiguration.h

    #internals
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3importer.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3importer.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3labelsimporter.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3labelsimporter.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/importconfiguration.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/importconfiguration.h
    
    ${CMAKE_CURRENT_LIST_DIR}/types/importtypes.h
)

# AU3
include(${CMAKE_CURRENT_LIST_DIR}/../../au3wrap/au3wrapDefs.cmake)

set(MODULE_INCLUDE ${AU3_INCLUDE})
set(MODULE_DEF ${AU3_DEF})
set(MODULE_LINK au3wrap)

set(MODULE_USE_UNITY OFF)

setup_module()
