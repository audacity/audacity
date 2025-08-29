#
# Audacity: A Digital Audio Editor
#

declare_module(exporter)

set(MODULE_QRC exporter.qrc)

set(MODULE_SRC
    # public
    ${CMAKE_CURRENT_LIST_DIR}/exportermodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/exportermodule.h
    ${CMAKE_CURRENT_LIST_DIR}/iexporter.h
    ${CMAKE_CURRENT_LIST_DIR}/iffmpegoptionsaccessor.h
    ${CMAKE_CURRENT_LIST_DIR}/iexportconfiguration.h

    #internals
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3exporter.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3exporter.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3ffmpegoptionsaccessor.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3ffmpegoptionsaccessor.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/exportconfiguration.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/exportconfiguration.h

    ${CMAKE_CURRENT_LIST_DIR}/view/exportpreferencesmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/exportpreferencesmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/dynamicexportoptionsmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/dynamicexportoptionsmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/customffmpegpreferencesmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/customffmpegpreferencesmodel.h

    #types
    ${CMAKE_CURRENT_LIST_DIR}/types/exporttypes.h

)

set(MODULE_QML_IMPORT ${CMAKE_CURRENT_LIST_DIR}/qml )

# AU3
include(${CMAKE_CURRENT_LIST_DIR}/../../au3wrap/au3defs.cmake)

set(MODULE_INCLUDE ${AU3_INCLUDE})
set(MODULE_DEF ${AU3_DEF})
set(MODULE_LINK au3wrap)

set(MODULE_USE_UNITY OFF)

setup_module()
