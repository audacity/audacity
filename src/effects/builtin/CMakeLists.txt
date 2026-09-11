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
    view/builtineffectsviewregister.cpp
    view/builtineffectsviewregister.h
    )

target_link_libraries(effects_builtin PRIVATE
        au3wrap
)

target_link_libraries(effects_builtin PUBLIC
        au3-effects
        au3-components # exposed by this module's public headers
)

add_subdirectory(qml/Audacity/BuiltinEffects)
