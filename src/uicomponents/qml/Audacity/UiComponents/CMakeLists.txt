muse_create_qml_module(uicomponents_qml FOR uicomponents)

qt_add_qml_module(uicomponents_qml
    URI Audacity.UiComponents
    VERSION 1.0
    SOURCES
        components/bpmmodel.cpp
        components/bpmmodel.h
        components/frequencymodel.cpp
        components/frequencymodel.h
        components/timecodemodel.cpp
        components/timecodemodel.h
        components/timecodemodeselector.cpp
        components/timecodemodeselector.h
        components/tablesortfilterproxymodel.cpp
        components/tablesortfilterproxymodel.h
        components/internal/numeric/numericviewmodel.cpp
        components/internal/numeric/numericviewmodel.h
        components/internal/numeric/timecodeformatter.cpp
        components/internal/numeric/timecodeformatter.h
        components/internal/numeric/numericformatter.cpp
        components/internal/numeric/numericformatter.h
        components/internal/numeric/beatsformatter.cpp
        components/internal/numeric/beatsformatter.h
        components/internal/numeric/fieldsinteractioncontroller.cpp
        components/internal/numeric/fieldsinteractioncontroller.h
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
