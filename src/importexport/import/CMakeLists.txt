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

# Loop Tempo Estimator (used for tempo detection on import)
# Configure sparse checkout so only source/ and LICENSE are fetched
set(_LTE_DIR ${CMAKE_SOURCE_DIR}/thirdparty/loop-tempo-estimator)
if(NOT EXISTS ${_LTE_DIR}/source/CMakeLists.txt)
    message(STATUS "Initializing loop-tempo-estimator submodule with sparse checkout...")
    execute_process(
        COMMAND git submodule init thirdparty/loop-tempo-estimator
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})
    execute_process(
        COMMAND git -C ${_LTE_DIR} sparse-checkout init --no-cone
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})
    execute_process(
        COMMAND git -C ${_LTE_DIR} sparse-checkout set /source/ /LICENSE
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})
    execute_process(
        COMMAND git submodule update thirdparty/loop-tempo-estimator
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})
endif()
if(NOT TARGET loop-tempo-estimator)
    add_subdirectory(${_LTE_DIR}/source ${CMAKE_BINARY_DIR}/loop-tempo-estimator)
endif()

# AU3
include(${CMAKE_CURRENT_LIST_DIR}/../../au3wrap/au3wrapDefs.cmake)

set(MODULE_INCLUDE ${AU3_INCLUDE})
set(MODULE_DEF ${AU3_DEF})
set(MODULE_LINK au3wrap loop-tempo-estimator)

set(MODULE_USE_UNITY OFF)

setup_module()
