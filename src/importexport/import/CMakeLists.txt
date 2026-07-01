#
# Audacity: A Digital Audio Editor
#

declare_module(importer)

set(MODULE_SRC
    # public
    ${CMAKE_CURRENT_LIST_DIR}/importermodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/importermodule.h
    ${CMAKE_CURRENT_LIST_DIR}/iimporter.h
    ${CMAKE_CURRENT_LIST_DIR}/iimporterconfiguration.h

    # internals
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3importer.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3importer.h

    ${CMAKE_CURRENT_LIST_DIR}/internal/importerconfiguration.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/importerconfiguration.h

    # tempo detection
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/tempodetection.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/tempodetection.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/lteaudioreaderbridge.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/lteaudioreaderbridge.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/filenamebpmparser.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/filenamebpmparser.h

    ${CMAKE_CURRENT_LIST_DIR}/types/importtypes.h
)

loop_tempo_estimator_add_to_build()

# AU3
include(${CMAKE_CURRENT_LIST_DIR}/../../au3wrap/au3wrapDefs.cmake)

set(MODULE_INCLUDE ${AU3_INCLUDE})
set(MODULE_DEF ${AU3_DEF})
set(MODULE_LINK au3wrap loop-tempo-estimator exporter)

set(MODULE_USE_UNITY OFF)

setup_module()
