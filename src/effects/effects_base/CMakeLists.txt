#
# Audacity: A Digital Audio Editor
#
declare_module(effects_base)

set(MODULE_QRC effects_base.qrc)

set(MODULE_QML_IMPORT ${CMAKE_CURRENT_LIST_DIR}/qml)

set(MODULE_SRC
    # public
    ${CMAKE_CURRENT_LIST_DIR}/effecterrors.h
    ${CMAKE_CURRENT_LIST_DIR}/effectsmodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/effectsmodule.h
    ${CMAKE_CURRENT_LIST_DIR}/effectstypes.h
    ${CMAKE_CURRENT_LIST_DIR}/ieffectsprovider.h
    ${CMAKE_CURRENT_LIST_DIR}/ieffectsconfiguration.h
    ${CMAKE_CURRENT_LIST_DIR}/ieffectsuiengine.h
    ${CMAKE_CURRENT_LIST_DIR}/ieffectsviewregister.h
    ${CMAKE_CURRENT_LIST_DIR}/ieffectinstancesregister.h
    ${CMAKE_CURRENT_LIST_DIR}/ieffectexecutionscenario.h
    ${CMAKE_CURRENT_LIST_DIR}/irealtimeeffectservice.h
    ${CMAKE_CURRENT_LIST_DIR}/ieffectpresetsprovider.h
    ${CMAKE_CURRENT_LIST_DIR}/ieffectpresetsscenario.h
    ${CMAKE_CURRENT_LIST_DIR}/ieffectviewlauncher.h
    ${CMAKE_CURRENT_LIST_DIR}/ieffectviewlaunchregister.h

    # internal
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectconfigsettings.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectconfigsettings.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectsconfiguration.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectsconfiguration.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectsactionscontroller.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectsactionscontroller.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectsuiactions.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectsuiactions.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectsprovider.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectsprovider.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectinstancesregister.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectinstancesregister.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectexecutionscenario.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectexecutionscenario.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/mastereffectundoredo.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/mastereffectundoredo.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/realtimeeffectservice.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/realtimeeffectservice.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/realtimeeffectserviceutils.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/realtimeeffectserviceutils.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectpresetsprovider.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectpresetsprovider.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectpresetsscenario.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectpresetsscenario.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectviewlaunchregister.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectviewlaunchregister.h

    # view
    ${CMAKE_CURRENT_LIST_DIR}/view/effectsviewregister.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/effectsviewregister.h
    ${CMAKE_CURRENT_LIST_DIR}/view/effectsuiengine.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/effectsuiengine.h
    ${CMAKE_CURRENT_LIST_DIR}/view/effectviewloader.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/effectviewloader.h
    ${CMAKE_CURRENT_LIST_DIR}/view/realtimeeffectviewerdialogmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/realtimeeffectviewerdialogmodel.h
    )

# AU3
include(${CMAKE_CURRENT_LIST_DIR}/../../au3wrap/au3defs.cmake)

set(MODULE_INCLUDE ${AU3_INCLUDE})
set(MODULE_DEF ${AU3_DEF})

set(MODULE_LINK au3wrap)

set(MODULE_USE_UNITY OFF)

setup_module()

if (AU_BUILD_EFFECTS_TESTS)
    add_subdirectory(tests)
endif()

