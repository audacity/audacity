#
# Audacity: A Digital Audio Editor
#
muse_create_module(effects_builtin NO_UNITY)

target_sources(effects_builtin PRIVATE
    # public
    builtineffectsmodule.cpp
    builtineffectsmodule.h
    ibuiltineffectsviewregister.h

    # internal
    internal/builtineffectsloader.h
    internal/builtineffectsmetareader.cpp
    internal/builtineffectsmetareader.h
    internal/builtineffectsscanner.h
    internal/builtintypes.h

    # view
    view/builtineffectinstanceaccess.cpp
    view/builtineffectinstanceaccess.h
    view/builtineffectmodel.cpp
    view/builtineffectmodel.h
    view/builtineffectviewloader.cpp
    view/builtineffectviewloader.h
    view/builtineffectsviewregister.cpp
    view/builtineffectsviewregister.h
    )

qt_add_resources(effects_builtin_RCC_SOURCES effects_builtin.qrc)
target_sources(effects_builtin PRIVATE ${effects_builtin_RCC_SOURCES})

add_qml_import_path(${CMAKE_CURRENT_LIST_DIR}/qml)

target_link_libraries(effects_builtin PRIVATE
        au3wrap
)

target_link_libraries(effects_builtin PUBLIC
        au3-effects
        au3-components # exposed by this module's public headers
)
