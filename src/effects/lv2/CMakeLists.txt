#
# Audacity: A Digital Audio Editor
#
declare_module(effects_lv2)

set(MODULE_QRC lv2.qrc)

set(MODULE_QML_IMPORT ${CMAKE_CURRENT_LIST_DIR}/qml )

set(MODULE_SRC
    ${CMAKE_CURRENT_LIST_DIR}/ilv2effectsrepository.h
    ${CMAKE_CURRENT_LIST_DIR}/lv2effectsmodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/lv2effectsmodule.h

    ${CMAKE_CURRENT_LIST_DIR}/internal/lv2effectsrepository.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/lv2effectsrepository.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/lv2pluginmetareader.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/lv2pluginmetareader.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/lv2pluginsscanner.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/lv2pluginsscanner.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/lv2viewlauncher.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/lv2viewlauncher.h

    ${CMAKE_CURRENT_LIST_DIR}/view/externalidleui.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/externalidleui.h
    ${CMAKE_CURRENT_LIST_DIR}/view/ilv2idleui.h
    ${CMAKE_CURRENT_LIST_DIR}/view/lv2uihandler.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/lv2uihandler.h
    ${CMAKE_CURRENT_LIST_DIR}/view/lv2idleuifactory.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/lv2idleuifactory.h
    ${CMAKE_CURRENT_LIST_DIR}/view/lv2viewmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/lv2viewmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/x11idleui.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/x11idleui.h
)

set(MODULE_LINK au3wrap lv2sdk X11)

# AU3
include(${CMAKE_CURRENT_LIST_DIR}/../../au3wrap/au3wrapDefs.cmake)

set(MODULE_DEF ${AU3_DEF})

set(MODULE_USE_UNITY OFF)
setup_module()
