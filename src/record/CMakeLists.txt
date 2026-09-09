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
    ${CMAKE_CURRENT_LIST_DIR}/irecordmetercontroller.h
    ${CMAKE_CURRENT_LIST_DIR}/irecord.h
    ${CMAKE_CURRENT_LIST_DIR}/iaudioinput.h

    #internal
    ${CMAKE_CURRENT_LIST_DIR}/internal/recordconfiguration.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/recordconfiguration.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/recordcontroller.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/recordcontroller.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/recordmetercontroller.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/recordmetercontroller.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/recorduiactions.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/recorduiactions.h

    #internal/au3
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3audioinput.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3audioinput.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3record.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3record.h

    ${CMAKE_CURRENT_LIST_DIR}/view/toolbars/playbacktoolbarrecordlevelitem.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/toolbars/playbacktoolbarrecordlevelitem.h
    ${CMAKE_CURRENT_LIST_DIR}/view/common/recordmetermodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/common/recordmetermodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/common/leadinrecordingindicatormodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/common/leadinrecordingindicatormodel.h
    )

set(MODULE_LINK
        au3wrap
        au3-audio-devices
        au3-audio-io
        au3-project-rate
        au3-stretching-sequence
        au3-strings
        au3-time-frequency-selection
        au3-track
        au3-wave-track
)

set(MODULE_USE_UNITY OFF)

setup_module()

if (AU_BUILD_RECORD_TESTS)
    add_subdirectory(tests)
endif()
