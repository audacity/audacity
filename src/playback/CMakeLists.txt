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
    ${CMAKE_CURRENT_LIST_DIR}/playbacktypes.h
    ${CMAKE_CURRENT_LIST_DIR}/iplayback.h
    ${CMAKE_CURRENT_LIST_DIR}/iplayer.h
    ${CMAKE_CURRENT_LIST_DIR}/iaudiooutput.h
    ${CMAKE_CURRENT_LIST_DIR}/iaudiodevicesprovider.h
    ${CMAKE_CURRENT_LIST_DIR}/iplaybackcontroller.h
    ${CMAKE_CURRENT_LIST_DIR}/iplaybackconfiguration.h
    ${CMAKE_CURRENT_LIST_DIR}/itrackplaybackcontrol.h

    #internals
    ${CMAKE_CURRENT_LIST_DIR}/internal/playbackconfiguration.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/playbackconfiguration.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/playbackcontroller.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/playbackcontroller.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/playbackuiactions.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/playbackuiactions.h

    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3playback.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3playback.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3player.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3player.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3audiooutput.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3audiooutput.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3audioinoutmeter.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3audioinoutmeter.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3trackplaybackcontrol.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3trackplaybackcontrol.h


    ${CMAKE_CURRENT_LIST_DIR}/view/common/playbackstatemodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/common/playbackstatemodel.h

    ${CMAKE_CURRENT_LIST_DIR}/view/toolbars/playbacktoolbarlevelitem.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/toolbars/playbacktoolbarlevelitem.h
    ${CMAKE_CURRENT_LIST_DIR}/view/toolbars/playbacktoolbarcontrolitem.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/toolbars/playbacktoolbarcontrolitem.h

    ${CMAKE_CURRENT_LIST_DIR}/view/toolbars/playbacktoolbartimeitem.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/toolbars/playbacktoolbartimeitem.h

    ${CMAKE_CURRENT_LIST_DIR}/view/toolbars/components/timecodemodeselector.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/toolbars/components/timecodemodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/toolbars/components/timecodemodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/toolbars/components/timecodeformatter.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/toolbars/components/timecodeformatter.h
    ${CMAKE_CURRENT_LIST_DIR}/view/toolbars/components/numericformatter.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/toolbars/components/numericformatter.h
    ${CMAKE_CURRENT_LIST_DIR}/view/toolbars/components/beatsformatter.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/toolbars/components/beatsformatter.h
    ${CMAKE_CURRENT_LIST_DIR}/view/toolbars/components/fieldsinteractioncontroller.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/toolbars/components/fieldsinteractioncontroller.h
    ${CMAKE_CURRENT_LIST_DIR}/view/toolbars/components/types.h

    ${CMAKE_CURRENT_LIST_DIR}/view/toolbars/playbacktoolbartimesignatureitem.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/toolbars/playbacktoolbartimesignatureitem.h

    ${CMAKE_CURRENT_LIST_DIR}/view/toolbars/playbacktoolbarbpmitem.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/toolbars/playbacktoolbarbpmitem.h

    ${CMAKE_CURRENT_LIST_DIR}/view/toolbars/components/bpmmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/toolbars/components/bpmmodel.h

    )

# AU3
include(${CMAKE_CURRENT_LIST_DIR}/../au3wrap/au3defs.cmake)

set(MODULE_INCLUDE ${AU3_INCLUDE})
set(MODULE_DEF ${AU3_DEF})
set(MODULE_LINK au3wrap)

set(MODULE_USE_UNITY OFF)

setup_module()

if (AU_BUILD_PLAYBACK_TESTS)
    add_subdirectory(tests)
endif()
