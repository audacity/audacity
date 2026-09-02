#
# Audacity: A Digital Audio Editor
#
declare_module(effects_lv2)

set(MODULE_QRC lv2.qrc)

set(MODULE_QML_IMPORT ${CMAKE_CURRENT_LIST_DIR}/qml )

set(MODULE_SRC
    ${CMAKE_CURRENT_LIST_DIR}/lv2effectsmodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/lv2effectsmodule.h

    ${CMAKE_CURRENT_LIST_DIR}/internal/lv2effectloader.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/lv2parameterextractorservice.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/lv2parameterextractorservice.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/lv2pluginmetareader.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/lv2pluginmetareader.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/lv2pluginsscanner.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/lv2uiselect.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/lv2uiselect.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/lv2viewlauncher.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/lv2viewlauncher.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/lv2types.h

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

set(MODULE_LINK
        effects_base
        au3wrap
        au3-components
        au3-effects
        au3-lv2
        au3-module-manager
        au3-realtime-effects
        X11
)

set(MODULE_USE_UNITY OFF)
setup_module()
