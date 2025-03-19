declare_module(effects_vst)

set(MODULE_QRC vst.qrc)
set(MODULE_QML_IMPORT ${CMAKE_CURRENT_LIST_DIR}/qml )
set(MODULE_IS_STUB ON)
set(MODULE_SRC
    ${CMAKE_CURRENT_LIST_DIR}/vsteffectsstubmodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/vsteffectsstubmodule.h
)

setup_module()
