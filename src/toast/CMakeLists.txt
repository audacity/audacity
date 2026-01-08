#
# Audacity: A Digital Audio Editor
#

declare_module(toast)

set(MODULE_QRC toast.qrc)

set(MODULE_SRC
    ${CMAKE_CURRENT_LIST_DIR}/itoast.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/toast.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/toast.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/toastprovider.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/toastprovider.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/toastitem.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/toastitem.h
    ${CMAKE_CURRENT_LIST_DIR}/view/toastlistmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/toastlistmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/toastmodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/toastmodule.h
)

set(MODULE_QML_IMPORT ${CMAKE_CURRENT_LIST_DIR}/qml)
set(MODULE_USE_UNITY OFF)

setup_module()