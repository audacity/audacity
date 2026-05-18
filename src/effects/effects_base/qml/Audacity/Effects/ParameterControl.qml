/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

import Audacity.Effects
import Audacity.UiComponents

Item {
    id: root

    property var parameterData: null
    property string parameterId: parameterData ? parameterData.id : ""

    // Properties for time controls
    property double sampleRate: 0
    property double tempo: 0
    property int upperTimeSignature: 0
    property int lowerTimeSignature: 0

    // Navigation: provided by the parent so Up/Down/Tab can be handled by the
    // inner controls (e.g. IncrementalPropertyControl's own NavigationEvent
    // handler) instead of bubbling up to the dialog ButtonBox.
    property NavigationPanel navigationPanel: null
    property int navigationOrderStart: 0

    signal valueChanged(string parameterId, double value)
    signal stringValueChanged(string parameterId, string stringValue)
    signal gestureStarted(string parameterId)
    signal gestureEnded(string parameterId)

    implicitHeight: rowLayout.implicitHeight

    QtObject {
        id: prv

        readonly property int spaceS: 4
        readonly property int spaceM: 8
        readonly property int spaceL: 12

        readonly property int labelWidth: 140
        readonly property int controlWidth: 140
        readonly property int numericControlWidth: 90
        readonly property int compactInputWidth: 80
        readonly property int valueDisplayWidth: 160
        readonly property int descriptionMaxWidth: 200
        readonly property int sliderMaxWidth: 280
        readonly property int sliderShortMaxWidth: 200
        readonly property int dropdownMaxWidth: 260
        readonly property int textControlWidth: 240
        readonly property int filePickerWidth: 320
    }

    RowLayout {
        id: rowLayout
        anchors.fill: parent
        anchors.rightMargin: prv.spaceL
        spacing: prv.spaceL

        // Parameter label
        StyledTextLabel {
            id: paramLabel
            Layout.preferredWidth: prv.labelWidth
            Layout.maximumWidth: prv.labelWidth
            Layout.alignment: Qt.AlignVCenter
            text: parameterData ? parameterData.name : ""
            horizontalAlignment: Text.AlignRight
            verticalAlignment: Text.AlignVCenter
            wrapMode: Text.WordWrap
        }

        // Control loader - loads different controls based on parameter type
        Loader {
            id: controlLoader
            Layout.alignment: Qt.AlignVCenter

            sourceComponent: {
                if (!parameterData) {
                    return null
                }

                switch (parameterData.type) {
                case "toggle":
                    return toggleControl
                case "dropdown":
                    return dropdownControl
                case "slider":
                    return sliderControl
                case "numeric":
                    return numericControl
                case "readonly":
                    return readonlyControl
                case "time":
                    return timeControl
                case "file":
                    return fileControl
                case "text":
                    return textControl
                default:
                    return unknownControl
                }
            }
        }

        // Optional human-readable hint, shared across all control types.
        // Capped width with word-wrap so long Nyquist labels stay readable.
        StyledTextLabel {
            id: descriptionLabel
            Layout.alignment: Qt.AlignVCenter
            Layout.maximumWidth: prv.descriptionMaxWidth
            text: parameterData ? parameterData.description : ""
            visible: text.length > 0
            horizontalAlignment: Text.AlignLeft
            verticalAlignment: Text.AlignVCenter
            wrapMode: Text.WordWrap
        }

        // Trailing fillWidth filler so the loader + description sit at their
        // content widths (left side of the row) rather than spreading out.
        Item {
            Layout.fillWidth: true
        }
    }

    // Toggle control (checkbox)
    Component {
        id: toggleControl

        CheckBox {
            navigation.panel: root.navigationPanel
            navigation.order: root.navigationOrderStart
            navigation.accessible.name: parameterData ? parameterData.name : ""
            navigation.accessible.description: parameterData ? parameterData.description : ""

            checked: parameterData ? parameterData.isToggleChecked : false
            enabled: parameterData ? !parameterData.isReadOnly : false

            onClicked: {
                // Toggle is a single atomic operation - begin and end gesture immediately
                root.gestureStarted(root.parameterId);

                // Send the opposite of current value (toggle)
                var isCurrentlyOn = parameterData.isToggleChecked
                root.valueChanged(root.parameterId, isCurrentlyOn ? parameterData.minValue : parameterData.maxValue)

                root.gestureEnded(root.parameterId)
            }
        }
    }

    // Dropdown control (combobox)
    Component {
        id: dropdownControl

        RowLayout {
            spacing: prv.spaceL

            StyledDropdown {
                id: dropdown
                Layout.maximumWidth: prv.dropdownMaxWidth
                Layout.alignment: Qt.AlignVCenter

                navigation.panel: root.navigationPanel
                navigation.order: root.navigationOrderStart
                navigation.accessible.name: parameterData ? parameterData.name + (dropdown.displayText ? ": " + dropdown.displayText : "") : ""
                navigation.accessible.description: parameterData ? parameterData.description : ""

                currentIndex: parameterData ? parameterData.currentEnumIndex : 0

                model: {
                    if (!parameterData || !parameterData.enumValues) {
                        return []
                    }

                    // Convert to array of objects with text property
                    var items = []
                    for (var i = 0; i < parameterData.enumValues.length; i++) {
                        items.push({
                            text: parameterData.enumValues[i]
                        })
                    }
                    return items
                }

                enabled: parameterData ? !parameterData.isReadOnly : false

                onActivated: function (index, value) {
                    // Dropdown selection is a single atomic operation - begin and end gesture immediately
                    root.gestureStarted(root.parameterId)

                    if (parameterData && parameterData.enumIndices && index >= 0 && index < parameterData.enumIndices.length) {
                        root.valueChanged(root.parameterId, parameterData.enumIndices[index])
                    }

                    root.gestureEnded(root.parameterId)
                }
            }
        }
    }

    // Slider control (continuous value)
    Component {
        id: sliderControl

        RowLayout {
            spacing: prv.spaceL

            StyledSlider {
                id: slider
                // Tighten the slider when there's a description label so the
                // hint text has room and doesn't get pushed off the row.
                Layout.preferredWidth: descriptionLabel.visible ? prv.sliderShortMaxWidth : prv.sliderMaxWidth
                Layout.minimumWidth: prv.controlWidth
                Layout.alignment: Qt.AlignVCenter

                navigation.panel: root.navigationPanel
                navigation.order: root.navigationOrderStart
                navigation.accessible.name: parameterData ? parameterData.name : ""
                navigation.accessible.description: parameterData ? parameterData.description : ""

                from: parameterData ? parameterData.minValue : 0
                to: parameterData ? parameterData.maxValue : 1
                value: parameterData ? parameterData.currentValue : 0
                stepSize: parameterData && parameterData.stepSize > 0 ? parameterData.stepSize : 0.01
                enabled: parameterData ? !parameterData.isReadOnly : false

                onPressedChanged: {
                    if (pressed) {
                        // Drag started - begin gesture
                        root.gestureStarted(root.parameterId)
                    } else {
                        // Drag ended - end gesture
                        root.gestureEnded(root.parameterId)
                    }
                }

                onMoved: {
                    root.valueChanged(root.parameterId, value)
                }
            }

            GeneratedIncrementalPropertyControl {
                Layout.preferredWidth: prv.compactInputWidth
                Layout.alignment: Qt.AlignVCenter
                parameterData: root.parameterData

                navigation.panel: root.navigationPanel
                navigation.order: slider.navigation.order + 1
                navigation.accessible.name: parameterData ? parameterData.name : ""
                navigation.accessible.description: parameterData ? parameterData.description : ""

                onGestureStarted: root.gestureStarted(root.parameterId)
                onGestureEnded: root.gestureEnded(root.parameterId)
                onValueCommitted: function (v) {
                    root.valueChanged(root.parameterId, v)
                }
            }
        }
    }

    // Numeric control (text input)
    Component {
        id: numericControl

        RowLayout {
            spacing: prv.spaceL

            GeneratedIncrementalPropertyControl {
                Layout.preferredWidth: prv.numericControlWidth
                Layout.alignment: Qt.AlignVCenter
                parameterData: root.parameterData

                navigation.panel: root.navigationPanel
                navigation.order: root.navigationOrderStart
                navigation.accessible.name: parameterData ? parameterData.name : ""
                navigation.accessible.description: parameterData ? parameterData.description : ""

                onGestureStarted: root.gestureStarted(root.parameterId)
                onGestureEnded: root.gestureEnded(root.parameterId)
                onValueCommitted: function (v) {
                    root.valueChanged(root.parameterId, v)
                }
            }
        }
    }

    // Time control (timecode input)
    Component {
        id: timeControl

        // Wrapper RowLayout + fillWidth filler so Timecode (NumericView, itself
        // a RowLayout) keeps its content-sized implicit width when the Loader
        // grows wider than its content. Without this, Timecode's internal
        // items (display + arrow-menu button) spread apart with a gap.
        RowLayout {
            spacing: 0

            Timecode {
                id: timecode
                Layout.alignment: Qt.AlignVCenter

                // Timecode (NumericView) does expose `navigation` -- as a
                // `property NavigationControl` instance, not a `property
                // alias`. Group-binding sub-properties on it still works.
                navigation.panel: root.navigationPanel
                navigation.order: root.navigationOrderStart
                navigation.accessible.description: parameterData ? parameterData.description : ""

                // NumericView composes accessible.name as `accessibleName +
                // valueString`; the parameter label is the prefix here.
                accessibleName: parameterData ? parameterData.name + ": " : ""

                value: parameterData ? parameterData.currentValue : 0
                mode: TimecodeModeSelector.Duration
                currentFormatStr: "" // TODO
                sampleRate: root.sampleRate
                tempo: root.tempo
                upperTimeSignature: root.upperTimeSignature
                lowerTimeSignature: root.lowerTimeSignature

                onValueChanged: {
                    root.valueChanged(root.parameterId, timecode.value)
                }

                onFocusChanged: {
                    if (focus) {
                        root.gestureStarted(root.parameterId)
                    } else {
                        root.gestureEnded(root.parameterId)
                    }
                }
            }

            Item {
                Layout.fillWidth: true
            }
        }
    }

    // File control (file picker)
    Component {
        id: fileControl

        RowLayout {
            spacing: prv.spaceL

            FilePicker {
                id: filePicker
                Layout.preferredWidth: prv.filePickerWidth
                Layout.alignment: Qt.AlignVCenter

                // FilePicker uses a different navigation API than the other
                // Muse controls: it takes the panel directly via `navigation`
                // (not `navigation.panel`) and accepts row/column orders.
                navigation: root.navigationPanel
                navigationRowOrderStart: root.navigationOrderStart

                // FilePicker prepends pathFieldTitle to its inner pathField's
                // accessible name, which is the only hook it exposes for the
                // parameter label.
                pathFieldTitle: parameterData ? parameterData.name : ""

                path: parameterData ? parameterData.currentValueString : ""

                pickerType: {
                    if (!parameterData) {
                        return FilePicker.PickerType.File
                    }

                    // If "save" flag is set, use Any type (save dialog)
                    if (parameterData.isFileSave) {
                        return FilePicker.PickerType.Any
                    }

                    // Otherwise use File type (open dialog)
                    // Note: Multiple file selection is not yet fully supported in the Framework
                    // but the flag is available for future implementation
                    return FilePicker.PickerType.File
                }

                enabled: parameterData ? !parameterData.isReadOnly : false

                // Set file filters from parameterData
                filter: {
                    if (!parameterData || !parameterData.fileFilters || parameterData.fileFilters.length === 0) {
                        return ""
                    }
                    // Join all filters with ";;" separator for Qt file dialog
                    return parameterData.fileFilters.join(";;")
                }

                onPathEdited: function (newPath) {
                    // File selection is a single atomic operation - begin and end gesture immediately
                    root.gestureStarted(root.parameterId)
                    root.stringValueChanged(root.parameterId, newPath)
                    root.gestureEnded(root.parameterId)
                }
            }
        }
    }

    // Text control (free-form string input)
    Component {
        id: textControl

        RowLayout {
            spacing: prv.spaceL

            TextInputField {
                id: textField
                Layout.preferredWidth: prv.textControlWidth
                Layout.alignment: Qt.AlignVCenter

                navigation.panel: root.navigationPanel
                navigation.order: root.navigationOrderStart
                navigation.accessible.name: parameterData ? parameterData.name + ": " + (textField.currentText || "") : ""
                navigation.accessible.description: parameterData ? parameterData.description : ""

                currentText: parameterData ? parameterData.currentValueString : ""
                enabled: parameterData ? !parameterData.isReadOnly : false

                onActiveFocusChanged: {
                    if (activeFocus) {
                        root.gestureStarted(root.parameterId)
                    } else {
                        root.gestureEnded(root.parameterId)
                    }
                }

                onTextEdited: function (newTextValue) {
                    root.stringValueChanged(root.parameterId, newTextValue)
                }
            }
        }
    }

    // Read-only control (display only)
    Component {
        id: readonlyControl

        StyledTextLabel {
            text: parameterData ? parameterData.formattedValue : ""
            opacity: 0.7
            wrapMode: Text.WordWrap
            horizontalAlignment: Text.AlignLeft
        }
    }

    // Unknown control type
    Component {
        id: unknownControl

        StyledTextLabel {
            text: qsTrc("effects", "Unknown parameter type: %1").arg(parameterData ? parameterData.type : "")
            opacity: 0.5
        }
    }
}
