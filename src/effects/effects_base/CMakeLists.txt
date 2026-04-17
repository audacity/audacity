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
    ${CMAKE_CURRENT_LIST_DIR}/ieffectviewcontroller.h
    ${CMAKE_CURRENT_LIST_DIR}/ieffectinstancesregister.h
    ${CMAKE_CURRENT_LIST_DIR}/ieffectexecutionscenario.h
    ${CMAKE_CURRENT_LIST_DIR}/irealtimeeffectservice.h
    ${CMAKE_CURRENT_LIST_DIR}/ieffectpresetsprovider.h
    ${CMAKE_CURRENT_LIST_DIR}/ieffectpresetsscenario.h
    ${CMAKE_CURRENT_LIST_DIR}/ieffectviewlauncher.h
    ${CMAKE_CURRENT_LIST_DIR}/ieffectviewlaunchregister.h
    ${CMAKE_CURRENT_LIST_DIR}/ieffectparametersprovider.h
    ${CMAKE_CURRENT_LIST_DIR}/iparameterextractorregistry.h
    ${CMAKE_CURRENT_LIST_DIR}/ipresetstatesregister.h
    ${CMAKE_CURRENT_LIST_DIR}/ieffectloader.h
    ${CMAKE_CURRENT_LIST_DIR}/ieffectloadersregister.h
    # interfaces for optional services
    ${CMAKE_CURRENT_LIST_DIR}/iparameterextractorservice.h

    # internal
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
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectsproviderinitializer.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectsproviderinitializer.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectsmenuprovider.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectsmenuprovider.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3effectloader.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3effectloader.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectloadersregister.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectloadersregister.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectsutils.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectsutils.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectviewcontroller.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectviewcontroller.h
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
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectparametersprovider.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectparametersprovider.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/parameterextractorregistry.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/parameterextractorregistry.h

    # AU3
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3audiopluginmetareader.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3audiopluginmetareader.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3audiopluginscanner.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3audiopluginscanner.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3effectsutils.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3/au3effectsutils.h

    # view
    ${CMAKE_CURRENT_LIST_DIR}/view/abstracteffectviewmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/abstracteffectviewmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/effectpresetsbarmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/effectpresetsbarmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/presetstatesregister.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/presetstatesregister.h
    ${CMAKE_CURRENT_LIST_DIR}/view/presetscontextmenumodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/presetscontextmenumodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/effectsavecontextmenu.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/effectsavecontextmenu.h
    ${CMAKE_CURRENT_LIST_DIR}/view/effectsuiengine.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/effectsuiengine.h
    ${CMAKE_CURRENT_LIST_DIR}/view/effectsviewtypes.h
    ${CMAKE_CURRENT_LIST_DIR}/view/effectsviewutils.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/effectsviewutils.h
    ${CMAKE_CURRENT_LIST_DIR}/view/destructiveeffectviewerdialogmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/destructiveeffectviewerdialogmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/realtimeeffectviewerdialogmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/realtimeeffectviewerdialogmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/effectparameterslistmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/effectparameterslistmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/generatedeffectviewermodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/generatedeffectviewermodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/pluginmanagersortfilterproxy.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/pluginmanagersortfilterproxy.h
    ${CMAKE_CURRENT_LIST_DIR}/view/pluginmanagertableviewmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/pluginmanagertableviewmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/pluginmanagertableviewverticalheader.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/pluginmanagertableviewverticalheader.h
    ${CMAKE_CURRENT_LIST_DIR}/view/dropdownoptionsmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/dropdownoptionsmodel.h
    )

# AU3
include(${CMAKE_CURRENT_LIST_DIR}/../../au3wrap/au3wrapDefs.cmake)

set(MODULE_INCLUDE ${AU3_INCLUDE})
set(MODULE_DEF ${AU3_DEF})

set(MODULE_LINK au3wrap uicomponents)

set(MODULE_USE_UNITY OFF)

setup_module()

if (AU_BUILD_EFFECTS_TESTS)
    add_subdirectory(tests)
endif()
