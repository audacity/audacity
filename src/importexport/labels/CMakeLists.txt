#
# Audacity: A Digital Audio Editor
#

declare_module(iex_labels)

set(MODULE_QRC labels.qrc)

set(MODULE_SRC
    # public
    ${CMAKE_CURRENT_LIST_DIR}/labelsmodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/labelsmodule.h
    ${CMAKE_CURRENT_LIST_DIR}/ilabelsimporter.h
    ${CMAKE_CURRENT_LIST_DIR}/ilabelsexporter.h
    ${CMAKE_CURRENT_LIST_DIR}/ilabelsconfiguration.h
    ${CMAKE_CURRENT_LIST_DIR}/labelsutils.h
    ${CMAKE_CURRENT_LIST_DIR}/labelstypes.h

    #internals
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3labelsimporter.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3labelsimporter.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3labelsexporter.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3labelsexporter.h

    ${CMAKE_CURRENT_LIST_DIR}/internal/labelsconfiguration.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/labelsconfiguration.h

    ${CMAKE_CURRENT_LIST_DIR}/view/exportlabelsmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/exportlabelsmodel.h
)

set(MODULE_QML_IMPORT ${CMAKE_CURRENT_LIST_DIR}/qml )

# AU3
include(${CMAKE_CURRENT_LIST_DIR}/../../au3wrap/au3wrapDefs.cmake)

set(MODULE_INCLUDE ${AU3_INCLUDE})
set(MODULE_DEF ${AU3_DEF})
set(MODULE_LINK au3wrap)

setup_module()
