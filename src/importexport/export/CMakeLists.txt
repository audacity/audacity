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
    ${CMAKE_CURRENT_LIST_DIR}/exportutils.cpp
    ${CMAKE_CURRENT_LIST_DIR}/exportutils.h

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
    ${CMAKE_CURRENT_LIST_DIR}/view/metadatamodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/metadatamodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/custommappingmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/custommappingmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/channelmappingtableviewmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/channelmappingtableviewmodel.h

    #types
    ${CMAKE_CURRENT_LIST_DIR}/types/exporttypes.h

)

set(MODULE_QML_IMPORT ${CMAKE_CURRENT_LIST_DIR}/qml )

set(MODULE_LINK
        au3wrap
        au3-audio-graph
        au3-basic-ui
        au3-channel
        au3-components
        au3-exceptions
        au3-files
        au3-import-export
        au3-math
        au3-mixer
        au3-playable-track
        au3-preferences
        au3-project-history
        au3-registries
        au3-sample-track
        au3-stretching-sequence
        au3-strings
        au3-tags
        au3-track
        au3-utility
        au3-wave-track
        au3-xml
)

set(MODULE_USE_UNITY OFF)

setup_module()
