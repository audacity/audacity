muse_create_qml_module(projectrecovery_qml FOR projectrecovery)

qt_add_qml_module(projectrecovery_qml
    URI Audacity.ProjectRecovery
    VERSION 1.0
    SOURCES
        projectrecoverywizardmodel.cpp
        projectrecoverywizardmodel.h
    QML_FILES
        ProjectRecoveryDialog.qml
        uicomponents/Callout.qml
        uicomponents/IndeterminateProgressBar.qml
        uicomponents/StepIndicator.qml
    IMPORTS
        TARGET muse_ui_qml
        TARGET muse_uicomponents_qml
)

fixup_qml_module_dependencies(projectrecovery_qml)

target_link_libraries(projectrecovery_qml PRIVATE Qt::Quick)
