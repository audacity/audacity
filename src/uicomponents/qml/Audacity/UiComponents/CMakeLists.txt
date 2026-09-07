muse_create_qml_module(uicomponents_qml FOR uicomponents)

qt_add_qml_module(uicomponents_qml
    URI Audacity.UiComponents
    VERSION 1.0
    SOURCES
        bpmmodel.cpp
        bpmmodel.h
        frequencymodel.cpp
        frequencymodel.h
        timecodemodel.cpp
        timecodemodel.h
        timecodemodeselector.cpp
        timecodemodeselector.h
        tablesortfilterproxymodel.cpp
        tablesortfilterproxymodel.h
        numericviewmodel.cpp
        numericviewmodel.h
        timecodeformatter.cpp
        timecodeformatter.h
        numericformatter.cpp
        numericformatter.h
        beatsformatter.cpp
        beatsformatter.h
        fieldsinteractioncontroller.cpp
        fieldsinteractioncontroller.h
    QML_FILES
        components/ArrowButton.qml
        components/ArrowMenuButton.qml
        components/BPM.qml
        components/BaseSection.qml
        components/DropdownWithTitle.qml
        components/Frequency.qml
        components/GridPlot.qml
        components/MarqueeSelection.qml
        components/RadioOptionsGroup.qml
        components/TimeSignature.qml
        components/Timecode.qml
        components/TimecodeStartEnd.qml
        components/ValueTooltip.qml
        components/internal/CornerHandle.qml
        components/internal/EdgeHandle.qml
        components/internal/NumericField.qml
        components/internal/NumericView.qml
        components/internal/TimeSignaturePopup.qml
    IMPORTS
        TARGET muse_ui_qml
        TARGET muse_uicomponents_qml
)

fixup_qml_module_dependencies(uicomponents_qml)
