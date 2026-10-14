muse_create_qml_module(macromanager_qml FOR macromanager NO_UNITY)

qt_add_qml_module(macromanager_qml
    URI Audacity.MacroManager
    VERSION 1.0
    SOURCES
        availablecommandsmodel.cpp
        availablecommandsmodel.h
        commandparamsmodel.cpp
        commandparamsmodel.h
        macroactions.cpp
        macroactions.h
        macrocommandsmodel.cpp
        macrocommandsmodel.h
        macromanagermodel.cpp
        macromanagermodel.h
        runonfileslauncher.cpp
        runonfileslauncher.h
        runonfilesmodel.cpp
        runonfilesmodel.h
    QML_FILES
        MacroManagerPanel.qml
        MacroManagerTitleBar.qml
        MacroNameDialog.qml
        DeleteMacroDialog.qml
        EditMacroDialog.qml
        CommandParamsDialog.qml
        RunOnFilesDialog.qml
        internal/MacroManagerHeader.qml
        internal/MacroItem.qml
        internal/MacroTitleBar.qml
        internal/CommandsSearchBar.qml
        internal/AvailableCommandsList.qml
        internal/MacroCommandsTable.qml
        internal/MacroCommandItem.qml
    IMPORTS
        TARGET muse_ui_qml
        TARGET muse_uicomponents_qml
)

fixup_qml_module_dependencies(macromanager_qml)
