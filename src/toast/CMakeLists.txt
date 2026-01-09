#
# Audacity: A Digital Audio Editor
#

declare_module(toast)

set(MODULE_QRC toast.qrc)

set(MODULE_SRC
    ${CMAKE_CURRENT_LIST_DIR}/itoastservice.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/toastservice.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/toastservice.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/toastprovider.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/toastprovider.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/toastitem.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/toastitem.h
    ${CMAKE_CURRENT_LIST_DIR}/view/toastlistmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/toastlistmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/dev/toasttestsmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/dev/toasttestsmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/toastmodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/toastmodule.h
)

set(MODULE_QML_IMPORT ${CMAKE_CURRENT_LIST_DIR}/qml)
set(MODULE_USE_UNITY OFF)

setup_module()
