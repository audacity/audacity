muse_create_qml_module(effects_vst_qml FOR effects_vst NO_UNITY)

qt_add_qml_module(effects_vst_qml
    URI Audacity.Vst
    VERSION 1.0
    SOURCES
        vstviewmodel.cpp
        vstviewmodel.h
    QML_FILES
        VstViewer.qml
    IMPORTS
        TARGET muse_ui_qml
        TARGET muse_uicomponents_qml
)

fixup_qml_module_dependencies(effects_vst_qml)

target_link_libraries(effects_vst_qml PRIVATE au3wrap au3-components au3-realtime-effects au3-vst3)
