muse_create_qml_module(effects_audio_unit_qml FOR effects_audio_unit STUB)

qt_add_qml_module(effects_audio_unit_qml
    URI Audacity.AudioUnit
    VERSION 1.0
    QML_FILES
        AudioUnitViewer.qml
    IMPORTS
        TARGET muse_ui_qml
        TARGET muse_uicomponents_qml
)

fixup_qml_module_dependencies(effects_audio_unit_qml)
