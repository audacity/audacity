#
# Audacity: A Digital Audio Editor
#
declare_module(effects_nyquist)

set(MODULE_QRC nyquist.qrc)

set(MODULE_QML_IMPORT ${CMAKE_CURRENT_LIST_DIR}/qml)

set(MODULE_SRC
    ${CMAKE_CURRENT_LIST_DIR}/nyquisteffectsmodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/nyquisteffectsmodule.h
    ${CMAKE_CURRENT_LIST_DIR}/inyquisteffectsrepository.h

    ${CMAKE_CURRENT_LIST_DIR}/internal/nyquisteffectsrepository.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/nyquisteffectsrepository.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/nyquistparameterextractorservice.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/nyquistparameterextractorservice.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/nyquistviewlauncher.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/nyquistviewlauncher.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/nyquistpluginsscanner.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/nyquistpluginsscanner.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/nyquistpluginsmetareader.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/nyquistpluginsmetareader.h

    ${CMAKE_CURRENT_LIST_DIR}/view/nyquistpromptviewmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/nyquistpromptviewmodel.h
    )

# AU3
include(${CMAKE_CURRENT_LIST_DIR}/../../au3wrap/au3wrapDefs.cmake)

set(MODULE_INCLUDE ${AU3_INCLUDE})
set(MODULE_DEF ${AU3_DEF})

set(MODULE_LINK au3wrap au3-nyquist-effects)

setup_module()
