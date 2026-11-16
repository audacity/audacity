muse_create_qml_module(effects_audio_unit_qml FOR effects_audio_unit)

qt_add_qml_module(effects_audio_unit_qml
    URI Audacity.AudioUnit
    VERSION 1.0
    SOURCES
        audiounitcontrol.h
        audiounitcontrol.mm
        audiounitview.cpp
        audiounitview.h
        audiounitviewmodel.cpp
        audiounitviewmodel.h
    QML_FILES
        AudioUnitViewer.qml
    IMPORTS
        TARGET muse_ui_qml
        TARGET muse_uicomponents_qml
)

set_source_files_properties(audiounitcontrol.mm PROPERTIES
    SKIP_UNITY_BUILD_INCLUSION ON
    SKIP_PRECOMPILE_HEADERS ON
)

fixup_qml_module_dependencies(effects_audio_unit_qml)

# AudioUnitView is a QQuickItem: link Qt::Quick explicitly instead of relying on the PCH
target_link_libraries(effects_audio_unit_qml PRIVATE
    Qt::Quick
    au3wrap
    au3-effects
    au3-realtime-effects
    au3-audio-unit
)
