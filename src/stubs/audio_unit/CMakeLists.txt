declare_module(effects_audio_unit)

set(MODULE_QRC audiounit.qrc)
set(MODULE_QML_IMPORT ${CMAKE_CURRENT_LIST_DIR}/qml )
set(MODULE_IS_STUB ON)
set(MODULE_SRC
    ${CMAKE_CURRENT_LIST_DIR}/audiouniteffectsstubmodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/audiouniteffectsstubmodule.h
)

setup_module()
