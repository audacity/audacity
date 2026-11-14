#
# Audacity: A Digital Audio Editor
#

declare_module(audacity_extensions)

set(MODULE_SRC
    ${CMAKE_CURRENT_LIST_DIR}/audacityextensionsmodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/audacityextensionsmodule.h
    ${CMAKE_CURRENT_LIST_DIR}/extensionpreferences.cpp
    ${CMAKE_CURRENT_LIST_DIR}/extensionpreferences.h
    ${CMAKE_CURRENT_LIST_DIR}/api/audiotransformapi.cpp
    ${CMAKE_CURRENT_LIST_DIR}/api/audiotransformapi.h
    ${CMAKE_CURRENT_LIST_DIR}/api/nativeapi.cpp
    ${CMAKE_CURRENT_LIST_DIR}/api/nativeapi.h
    ${CMAKE_CURRENT_LIST_DIR}/api/preferencesapi.cpp
    ${CMAKE_CURRENT_LIST_DIR}/api/preferencesapi.h
)

set(MODULE_LINK trackedit extension_native muse::extensions)
set(MODULE_USE_UNITY OFF)

setup_module()
