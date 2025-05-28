declare_module(effects_lv2)

set(MODULE_QRC lv2.qrc)
set(MODULE_QML_IMPORT ${CMAKE_CURRENT_LIST_DIR}/qml )
set(MODULE_IS_STUB ON)
set(MODULE_SRC
    ${CMAKE_CURRENT_LIST_DIR}/lv2effectsstubmodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/lv2effectsstubmodule.h
)

setup_module()
