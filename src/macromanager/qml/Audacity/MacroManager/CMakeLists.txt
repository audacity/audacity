muse_create_qml_module(macromanager_qml FOR macromanager NO_UNITY)

qt_add_qml_module(macromanager_qml
    URI Audacity.MacroManager
    VERSION 1.0
    SOURCES
        macromanagermodel.cpp
        macromanagermodel.h
    QML_FILES
        MacroManagerPanel.qml
        MacroManagerTitleBar.qml
        NewMacroDialog.qml
        internal/MacroManagerHeader.qml
        internal/MacroItem.qml
    IMPORTS
        TARGET muse_ui_qml
        TARGET muse_uicomponents_qml
)

fixup_qml_module_dependencies(macromanager_qml)
