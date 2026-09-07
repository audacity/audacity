muse_create_qml_module(effects_builtin_qml FOR effects_builtin NO_UNITY)

qt_add_qml_module(effects_builtin_qml
    URI Audacity.BuiltinEffects
    VERSION 1.0
    SOURCES
        builtineffectinstanceaccess.cpp
        builtineffectinstanceaccess.h
        builtineffectmodel.cpp
        builtineffectmodel.h
        builtineffectviewloader.cpp
        builtineffectviewloader.h
    QML_FILES
        BuiltinEffectBase.qml
        BuiltinEffectViewer.qml
)

fixup_qml_module_dependencies(effects_builtin_qml)

target_link_libraries(effects_builtin_qml PRIVATE au3wrap)
