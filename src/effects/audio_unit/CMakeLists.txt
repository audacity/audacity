#
# Audacity: A Digital Audio Editor
#
declare_module(effects_audio_unit)

set(MODULE_QRC audio_unit.qrc)

set(MODULE_QML_IMPORT ${CMAKE_CURRENT_LIST_DIR}/qml)

set(MODULE_SRC
    ${CMAKE_CURRENT_LIST_DIR}/audiouniteffectsmodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/audiouniteffectsmodule.h
    ${CMAKE_CURRENT_LIST_DIR}/iaudiouniteffectsrepository.h

    ${CMAKE_CURRENT_LIST_DIR}/internal/audiouniteffectsrepository.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/audiouniteffectsrepository.h

    ${CMAKE_CURRENT_LIST_DIR}/internal/audiounitpluginsscanner.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/audiounitpluginsscanner.h

    ${CMAKE_CURRENT_LIST_DIR}/internal/audiounitpluginsmetareader.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/audiounitpluginsmetareader.h

    ${CMAKE_CURRENT_LIST_DIR}/internal/audiounitviewlauncher.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/audiounitviewlauncher.h

    ${CMAKE_CURRENT_LIST_DIR}/view/audiounitview.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/audiounitview.h
    ${CMAKE_CURRENT_LIST_DIR}/view/audiounitviewmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/audiounitviewmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/audiounitcontrol.mm
    ${CMAKE_CURRENT_LIST_DIR}/view/audiounitcontrol.h
)

set_source_files_properties(
    ${CMAKE_CURRENT_LIST_DIR}/view/audiounitcontrol.mm
    PROPERTIES
    SKIP_UNITY_BUILD_INCLUSION ON
    SKIP_PRECOMPILE_HEADERS ON
)

# AU3
include(${CMAKE_CURRENT_LIST_DIR}/../../au3wrap/au3defs.cmake)

set(MODULE_INCLUDE ${AU3_INCLUDE})
set(MODULE_DEF ${AU3_DEF})

set(MODULE_LINK au3wrap)

setup_module()
