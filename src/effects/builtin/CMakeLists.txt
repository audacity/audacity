#
# Audacity: A Digital Audio Editor
#
declare_module(effects_builtin)

set(MODULE_QRC effects_builtin.qrc)

set(MODULE_QML_IMPORT ${CMAKE_CURRENT_LIST_DIR}/qml)

set(MODULE_SRC
    # public
    ${CMAKE_CURRENT_LIST_DIR}/builtineffectsmodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/builtineffectsmodule.h
    ${CMAKE_CURRENT_LIST_DIR}/ibuiltineffectsrepository.h
    ${CMAKE_CURRENT_LIST_DIR}/ibuiltineffectsviewregister.h

    # internal
    ${CMAKE_CURRENT_LIST_DIR}/internal/builtineffectsrepository.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/builtineffectsrepository.h

    # view
    ${CMAKE_CURRENT_LIST_DIR}/view/builtineffectmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/builtineffectmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/builtineffectviewloader.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/builtineffectviewloader.h
    ${CMAKE_CURRENT_LIST_DIR}/view/builtineffectsviewregister.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/builtineffectsviewregister.h
    )

# AU3
include(${CMAKE_CURRENT_LIST_DIR}/../../au3wrap/au3wrapDefs.cmake)

set(MODULE_INCLUDE ${AU3_INCLUDE})
set(MODULE_DEF ${AU3_DEF})

set(MODULE_LINK au3wrap)

set(MODULE_USE_UNITY OFF)

setup_module()
