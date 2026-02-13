declare_module(effects_nyquist)

set(MODULE_QRC nyquist.qrc)
set(MODULE_QML_IMPORT ${CMAKE_CURRENT_LIST_DIR}/qml )
set(MODULE_IS_STUB ON)
set(MODULE_SRC
    ${CMAKE_CURRENT_LIST_DIR}/nyquisteffectsstubmodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/nyquisteffectsstubmodule.h
)

setup_module()

