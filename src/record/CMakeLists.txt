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
    ${CMAKE_CURRENT_LIST_DIR}/recorderrors.h
    ${CMAKE_CURRENT_LIST_DIR}/irecordconfiguration.h
    ${CMAKE_CURRENT_LIST_DIR}/irecordcontroller.h
    ${CMAKE_CURRENT_LIST_DIR}/irecord.h
    ${CMAKE_CURRENT_LIST_DIR}/iaudioinput.h

    #internal
    ${CMAKE_CURRENT_LIST_DIR}/internal/recordconfiguration.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/recordconfiguration.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/recordcontroller.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/recordcontroller.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/recorduiactions.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/recorduiactions.h

    #internal/au3
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3audioinput.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3audioinput.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3record.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3record.h

    ${CMAKE_CURRENT_LIST_DIR}/view/toolbars/playbacktoolbarrecordlevelitem.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/toolbars/playbacktoolbarrecordlevelitem.h
    )

# AU3
include(${CMAKE_CURRENT_LIST_DIR}/../au3wrap/au3defs.cmake)

set(MODULE_INCLUDE ${AU3_INCLUDE})
set(MODULE_DEF ${AU3_DEF})
set(MODULE_LINK au3wrap)

set(MODULE_USE_UNITY OFF)

setup_module()

if (AU_BUILD_RECORD_TESTS)
    add_subdirectory(tests)
endif()
