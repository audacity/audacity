declare_module(audacity_extensions)

set(MODULE_IS_STUB ON)
set(MODULE_SRC
    ${CMAKE_CURRENT_LIST_DIR}/audacityextensionsstubmodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/audacityextensionsstubmodule.h
    ${CMAKE_CURRENT_LIST_DIR}/../../extensions/extensionpreferences.cpp
    ${CMAKE_CURRENT_LIST_DIR}/../../extensions/extensionpreferences.h
)
set(MODULE_LINK muse::extensions)

setup_module()
