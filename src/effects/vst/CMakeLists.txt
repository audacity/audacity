#
# Audacity: A Digital Audio Editor
#
declare_module(effects_vst)

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

    # from muse
    ${MUSE_FRAMEWORK_SRC_PATH}/vst/view/abstractvsteditorview.cpp
    ${MUSE_FRAMEWORK_SRC_PATH}/vst/view/abstractvsteditorview.h
    ${MUSE_FRAMEWORK_SRC_PATH}/vst/view/vstfxeditorview.cpp
    ${MUSE_FRAMEWORK_SRC_PATH}/vst/view/vstfxeditorview.h

    # for muse
    ${CMAKE_CURRENT_LIST_DIR}/internal/musevstplugininstance.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/musevstplugininstance.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/musevstpluginsregister.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/musevstpluginsregister.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/musevstmodulesrepository.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/musevstmodulesrepository.h
    )

# AU3
include(${CMAKE_CURRENT_LIST_DIR}/../../au3wrap/au3defs.cmake)

set(MODULE_INCLUDE
    ${AU3_INCLUDE}
    ${MUSE_FRAMEWORK_SRC_PATH}/vst
)
set(MODULE_DEF ${AU3_DEF})

set(MODULE_LINK au3wrap vst_sdk_3)

setup_module()
