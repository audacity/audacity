# SPDX-License-Identifier: GPL-3.0-only

declare_module(audacityplugin)

set(MODULE_SRC
    ${CMAKE_CURRENT_LIST_DIR}/iaudacitypluginhost.h
    ${CMAKE_CURRENT_LIST_DIR}/audacityplugintypes.h
    ${CMAKE_CURRENT_LIST_DIR}/audacitypluginmodule.h
    ${CMAKE_CURRENT_LIST_DIR}/audacitypluginmodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/abi.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/abi.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/manifestreader.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/manifestreader.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/pluginlibrary.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/pluginlibrary.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/loadedplugin.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/loadedplugin.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectinstance.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/effectinstance.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/offlineeffectadapter.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/offlineeffectadapter.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/audacitypluginhost.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/audacitypluginhost.cpp
)

set(MODULE_INCLUDE
    ${PROJECT_SOURCE_DIR}/sdk/audacityplugin/v0
)

set(MODULE_LINK
    ${CMAKE_DL_LIBS}
)

setup_module()

install(FILES
    ${PROJECT_SOURCE_DIR}/sdk/audacityplugin/v0/audacity_plugin_v0.h
    DESTINATION include/audacityplugin/v0
    COMPONENT audacityplugin
)
install(DIRECTORY
    ${PROJECT_SOURCE_DIR}/sdk/audacityplugin/v0/examples
    DESTINATION share/audacityplugin/v0
    COMPONENT audacityplugin
)
install(FILES
    ${PROJECT_SOURCE_DIR}/sdk/audacityplugin/v0/plugin.schema.json
    DESTINATION share/audacityplugin/v0
    COMPONENT audacityplugin
)
