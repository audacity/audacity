#
# Audacity: A Digital Audio Editor
#
declare_module(effects_lv2)

#set(MODULE_QRC lv2.qrc)

set(MODULE_QML_IMPORT ${CMAKE_CURRENT_LIST_DIR}/qml )

set(MODULE_SRC
    ${CMAKE_CURRENT_LIST_DIR}/lv2effectsmodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/lv2effectsmodule.h
)

# AU3
include(${CMAKE_CURRENT_LIST_DIR}/../../au3wrap/au3defs.cmake)

set(MODULE_INCLUDE
    ${AU3_INCLUDE}
    ${MUSE_FRAMEWORK_SRC_PATH}/vst
)
set(MODULE_DEF ${AU3_DEF})

set(MODULE_LINK au3wrap)

set(MODULE_USE_UNITY OFF)
setup_module()
