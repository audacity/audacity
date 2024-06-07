#
# Audacity: A Digital Audio Editor
#

declare_module(playback)

set(MODULE_QRC playback.qrc)
set(MODULE_QML_IMPORT ${CMAKE_CURRENT_LIST_DIR}/qml)

set(MODULE_SRC
    #public
    ${CMAKE_CURRENT_LIST_DIR}/playbackmodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/playbackmodule.h
    ${CMAKE_CURRENT_LIST_DIR}/audiotypes.h
    ${CMAKE_CURRENT_LIST_DIR}/iplayback.h
    ${CMAKE_CURRENT_LIST_DIR}/iplayer.h
    ${CMAKE_CURRENT_LIST_DIR}/iaudiooutput.h
    ${CMAKE_CURRENT_LIST_DIR}/iplaybackcontroller.h

    #internals
    ${CMAKE_CURRENT_LIST_DIR}/internal/playbackcontroller.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/playbackcontroller.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/playbackuiactions.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/playbackuiactions.h

    ${CMAKE_CURRENT_LIST_DIR}/view/toolbars/playbacktoolbarmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/toolbars/playbacktoolbarmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/toolbars/playbacktoolbarlevelitem.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/toolbars/playbacktoolbarlevelitem.h

    ${CMAKE_CURRENT_LIST_DIR}/view/toolbars/playbacktoolbarcustomisemodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/toolbars/playbacktoolbarcustomisemodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/toolbars/playbacktoolbarcustomiseitem.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/toolbars/playbacktoolbarcustomiseitem.h

    )

set(LINK_LIB
    record
)

setup_module()
