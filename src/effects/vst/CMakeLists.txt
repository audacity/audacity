#
# Audacity: A Digital Audio Editor
#
declare_module(effects_vst)

set(MODULE_QRC vst.qrc)

set(MODULE_QML_IMPORT ${CMAKE_CURRENT_LIST_DIR}/qml )

set(MODULE_SRC
    ${CMAKE_CURRENT_LIST_DIR}/vsteffectsmodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/vsteffectsmodule.h

    ${CMAKE_CURRENT_LIST_DIR}/internal/vst3effectloader.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/vst3pluginsscanner.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/vst3pluginsmetareader.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/vst3pluginsmetareader.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/vst3viewlauncher.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/vst3viewlauncher.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/vstparameterextractorservice.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/vstparameterextractorservice.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/vsttypes.h

    ${CMAKE_CURRENT_LIST_DIR}/view/vstviewmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/vstviewmodel.h

    # for muse
    ${CMAKE_CURRENT_LIST_DIR}/internal/musevstplugininstance.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/musevstplugininstance.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/musevstpluginsregister.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/musevstpluginsregister.h
)

set(MODULE_LINK
        au3wrap
        au3-components
        au3-realtime-effects
        au3-vst3
)

if(MUSE_MODULE_VST_QML)
    list(APPEND MODULE_LINK
            muse::vst_qml
    )
endif()

set(MODULE_USE_UNITY OFF)
setup_module()
