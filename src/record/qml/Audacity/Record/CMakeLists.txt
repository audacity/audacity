muse_create_qml_module(record_qml FOR record NO_UNITY)

qt_add_qml_module(record_qml
    URI Audacity.Record
    VERSION 1.0
    SOURCES
        recordmetermodel.cpp
        recordmetermodel.h
        leadinrecordingindicatormodel.cpp
        leadinrecordingindicatormodel.h
    QML_FILES
        RecordLevel.qml
        internal/RecordLevelPopup.qml
    IMPORTS
        TARGET muse_ui_qml
        TARGET muse_uicomponents_qml
)

fixup_qml_module_dependencies(record_qml)

target_link_libraries(record_qml PRIVATE playback)
