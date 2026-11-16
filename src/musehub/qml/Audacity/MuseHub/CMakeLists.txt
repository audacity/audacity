muse_create_qml_module(musehub_qml FOR musehub)

qt_add_qml_module(musehub_qml
    URI Audacity.MuseHub
    VERSION 1.0
    SOURCES
        geteffectsmodel.cpp
        geteffectsmodel.h
    QML_FILES
        GetEffectsDialog.qml
        GetEffectsMenu.qml
        internal/GetEffectsContent.qml
        internal/EffectCard.qml
    IMPORTS
        TARGET muse_ui_qml
        TARGET muse_uicomponents_qml
)

fixup_qml_module_dependencies(musehub_qml)

target_link_libraries(musehub_qml PRIVATE au3-musehub Qt::Quick)
