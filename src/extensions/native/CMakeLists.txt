declare_module(extension_native)

set(MODULE_SRC
    ${CMAKE_CURRENT_LIST_DIR}/bytebuffer.cpp
    ${CMAKE_CURRENT_LIST_DIR}/bytebuffer.h
    ${CMAKE_CURRENT_LIST_DIR}/nativeextension.h
    ${CMAKE_CURRENT_LIST_DIR}/nativelibrary.cpp
    ${CMAKE_CURRENT_LIST_DIR}/nativelibrary.h
)

set(MODULE_INCLUDE ${PROJECT_SOURCE_DIR}/src)
set(MODULE_LINK_PUBLIC Qt::Core Qt::Qml)
set(MODULE_USE_UNITY OFF)

setup_module()
