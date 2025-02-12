#
# Audacity: A Digital Audio Editor
#
declare_module(effects_vst)

set(MODULE_QRC vst.qrc)

set(MODULE_QML_IMPORT ${CMAKE_CURRENT_LIST_DIR}/qml )

set(MODULE_SRC
    ${CMAKE_CURRENT_LIST_DIR}/vsteffectsmodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/vsteffectsmodule.h
    ${CMAKE_CURRENT_LIST_DIR}/ivsteffectsrepository.h

    ${CMAKE_CURRENT_LIST_DIR}/internal/vsteffectsrepository.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/vsteffectsrepository.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/vst3pluginsscanner.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/vst3pluginsscanner.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/vst3pluginsmetareader.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/vst3pluginsmetareader.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/vst3viewlauncher.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/vst3viewlauncher.h

    ${CMAKE_CURRENT_LIST_DIR}/view/vstviewmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/vstviewmodel.h

    # from muse
    ${MUSE_FRAMEWORK_SRC_PATH}/vst/view/vstview.cpp
    ${MUSE_FRAMEWORK_SRC_PATH}/vst/view/vstview.h

    # for muse
    ${CMAKE_CURRENT_LIST_DIR}/internal/musevstplugininstance.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/musevstplugininstance.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/musevstpluginsregister.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/musevstpluginsregister.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/musevstmodulesrepository.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/musevstmodulesrepository.h
)

if(OS_IS_LIN OR OS_IS_FBSD)
    set(MODULE_SRC
        ${MODULE_SRC}
        ${MUSE_FRAMEWORK_SRC_PATH}/vst/internal/platform/linux/runloop.cpp
        ${MUSE_FRAMEWORK_SRC_PATH}/vst/internal/platform/linux/runloop.h
    )
endif()

# AU3
include(${CMAKE_CURRENT_LIST_DIR}/../../au3wrap/au3defs.cmake)

set(MODULE_INCLUDE
    ${AU3_INCLUDE}
    ${MUSE_FRAMEWORK_SRC_PATH}/vst
)
set(MODULE_DEF ${AU3_DEF})

set(MODULE_LINK au3wrap vst_sdk_3)

set(MODULE_USE_UNITY OFF)
setup_module()
