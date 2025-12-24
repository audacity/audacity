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
    ${CMAKE_CURRENT_LIST_DIR}/ieffectsmenuprovider.h
    ${CMAKE_CURRENT_LIST_DIR}/ieffectsconfiguration.h
    ${CMAKE_CURRENT_LIST_DIR}/ieffectsuiengine.h
    ${CMAKE_CURRENT_LIST_DIR}/ieffectinstancesregister.h
    ${CMAKE_CURRENT_LIST_DIR}/ieffectexecutionscenario.h
    ${CMAKE_CURRENT_LIST_DIR}/irealtimeeffectservice.h
    ${CMAKE_CURRENT_LIST_DIR}/ieffectpresetsprovider.h
    ${CMAKE_CURRENT_LIST_DIR}/ieffectpresetsscenario.h
    ${CMAKE_CURRENT_LIST_DIR}/ieffectviewlauncher.h
    ${CMAKE_CURRENT_LIST_DIR}/ieffectviewlaunchregister.h

    # internal
    ${CMAKE_CURRENT_LIST_DIR}/internal/abstractaudiopluginmetareader.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/abstractaudiopluginmetareader.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/abstractviewlauncher.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/abstractviewlauncher.h
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
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectsmenuprovider.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectsmenuprovider.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectsrepositoryhelper.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectsrepositoryhelper.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectsutils.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectsutils.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectinstancesregister.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectinstancesregister.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectexecutionscenario.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectexecutionscenario.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/realtimeeffectrestorer.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/realtimeeffectrestorer.h
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
    ${CMAKE_CURRENT_LIST_DIR}/view/abstracteffectviewmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/abstracteffectviewmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/effectmanagemenu.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/effectmanagemenu.h
    ${CMAKE_CURRENT_LIST_DIR}/view/effectsuiengine.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/effectsuiengine.h
    ${CMAKE_CURRENT_LIST_DIR}/view/effectsviewutils.h
    ${CMAKE_CURRENT_LIST_DIR}/view/destructiveeffectviewerdialogmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/destructiveeffectviewerdialogmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/realtimeeffectviewerdialogmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/realtimeeffectviewerdialogmodel.h
    )

# VST3 parameter extraction (conditional)
if(AU_MODULE_EFFECTS_VST)
    list(APPEND MODULE_SRC
        ${CMAKE_CURRENT_LIST_DIR}/internal/vst3/vst3parametersextractor.cpp
        ${CMAKE_CURRENT_LIST_DIR}/internal/vst3/vst3parametersextractor.h
    )
endif()

# AU3
include(${CMAKE_CURRENT_LIST_DIR}/../../au3wrap/au3wrapDefs.cmake)

set(MODULE_INCLUDE ${AU3_INCLUDE})
set(MODULE_DEF ${AU3_DEF})

# Add USE_VST3 define if VST3 is enabled
# Note: AU4 code uses VST3ParameterExtraction bridge from au3-vst3 which does not expose VST3 SDK types,
# so we don't need to add the VST3 SDK include path here.
if(AU_MODULE_EFFECTS_VST)
    list(APPEND MODULE_DEF -DUSE_VST3)
endif()

set(MODULE_LINK au3wrap)

set(MODULE_USE_UNITY OFF)

setup_module()

if (AU_BUILD_EFFECTS_TESTS)
    add_subdirectory(tests)
endif()

