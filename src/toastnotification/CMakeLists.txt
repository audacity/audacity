#
# Audacity: A Digital Audio Editor
#

declare_module(toastnotification)

set(MODULE_QRC toastnotification.qrc)

set(MODULE_SRC
    ${CMAKE_CURRENT_LIST_DIR}/itoastnotification.h
    ${CMAKE_CURRENT_LIST_DIR}/toastnotification.cpp
    ${CMAKE_CURRENT_LIST_DIR}/toastnotification.h
    ${CMAKE_CURRENT_LIST_DIR}/toastnotificationmodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/toastnotificationmodule.h
    ${CMAKE_CURRENT_LIST_DIR}/toastnotificationitembuilder.cpp
    ${CMAKE_CURRENT_LIST_DIR}/toastnotificationitembuilder.h
    ${CMAKE_CURRENT_LIST_DIR}/toastnotificationtypes.h

    ${CMAKE_CURRENT_LIST_DIR}/internal/toastnotificationuiactions.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/toastnotificationuiactions.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/toastnotificationcontroller.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/toastnotificationcontroller.h

    ${CMAKE_CURRENT_LIST_DIR}/view/toastnotificationlistmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/toastnotificationlistmodel.h
)

set(MODULE_QML_IMPORT ${CMAKE_CURRENT_LIST_DIR}/qml)
set(MODULE_USE_UNITY OFF)

setup_module()