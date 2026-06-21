#
# Audacity: A Digital Audio Editor
#

declare_module(videopreview)

set(MODULE_QRC videopreview.qrc)

set(MODULE_SRC
    ${CMAKE_CURRENT_LIST_DIR}/videopreviewmodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/videopreviewmodule.h
    ${CMAKE_CURRENT_LIST_DIR}/ivideopreviewservice.h
    ${CMAKE_CURRENT_LIST_DIR}/videopreviewtypes.h

    ${CMAKE_CURRENT_LIST_DIR}/internal/ffmpegvideodecoder.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/ffmpegvideodecoder.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/videopreviewservice.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/videopreviewservice.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/videolinkstorage.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/videolinkstorage.h

    ${CMAKE_CURRENT_LIST_DIR}/view/videopreviewitem.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/videopreviewitem.h
    ${CMAKE_CURRENT_LIST_DIR}/view/videopreviewmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/videopreviewmodel.h
)

set(MODULE_QML_IMPORT ${CMAKE_CURRENT_LIST_DIR}/qml)

include(${CMAKE_CURRENT_LIST_DIR}/../au3wrap/au3wrapDefs.cmake)

set(MODULE_INCLUDE ${AU3_INCLUDE})
set(MODULE_DEF ${AU3_DEF})
set(MODULE_LINK au3wrap context trackedit uicomponents Qt::Gui Qt::Quick)

set(MODULE_USE_UNITY OFF)

setup_module()
