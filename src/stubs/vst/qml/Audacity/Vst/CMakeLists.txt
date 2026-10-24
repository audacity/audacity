muse_create_qml_module(effects_vst_qml FOR effects_vst STUB)

qt_add_qml_module(effects_vst_qml
    URI Audacity.Vst
    VERSION 1.0
    QML_FILES
        VstViewer.qml
    IMPORTS
        TARGET muse_ui_qml
        TARGET muse_uicomponents_qml
)

fixup_qml_module_dependencies(effects_vst_qml)
