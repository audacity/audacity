declare_module(video)

set(MODULE_QRC video.qrc)
set(MODULE_QML_IMPORT ${CMAKE_CURRENT_LIST_DIR}/qml)
set(MODULE_IS_STUB ON)
set(MODULE_SRC
    ${CMAKE_CURRENT_LIST_DIR}/videostubmodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/videostubmodule.h
)

setup_module()
