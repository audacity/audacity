#
# Audacity: A Digital Audio Editor
#

declare_module(record)

set(MODULE_QRC record.qrc)
set(MODULE_QML_IMPORT ${CMAKE_CURRENT_LIST_DIR}/qml)

set(MODULE_SRC
    #public
    ${CMAKE_CURRENT_LIST_DIR}/recordmodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/recordmodule.h
    ${CMAKE_CURRENT_LIST_DIR}/irecordcontroller.h

    #internals
    ${CMAKE_CURRENT_LIST_DIR}/internal/recordcontroller.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/recordcontroller.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/recorduiactions.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/recorduiactions.h

    ${CMAKE_CURRENT_LIST_DIR}/view/toolbars/playbacktoolbarrecordlevelitem.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/toolbars/playbacktoolbarrecordlevelitem.h
    )

setup_module()
