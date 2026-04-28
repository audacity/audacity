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

# Loop Tempo Estimator (fetched automatically during configure)
if(NOT TARGET loop-tempo-estimator)
    set(LTE_VERSION "v0.0.4")

    include(FetchContent)
    FetchContent_Declare(
        loop-tempo-estimator
        URL https://github.com/saintmatthieu/loop-tempo-estimator/releases/download/${LTE_VERSION}/loop-tempo-estimator-${LTE_VERSION}.tar.gz
        SOURCE_SUBDIR source
    )
    FetchContent_MakeAvailable(loop-tempo-estimator)
endif()

# AU3
include(${CMAKE_CURRENT_LIST_DIR}/../../au3wrap/au3wrapDefs.cmake)

set(MODULE_INCLUDE ${AU3_INCLUDE})
set(MODULE_DEF ${AU3_DEF})
set(MODULE_LINK au3wrap loop-tempo-estimator)

set(MODULE_USE_UNITY OFF)

setup_module()
