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

    signal valueChanged(string parameterId, double value)
    signal gestureStarted(string parameterId)
    signal gestureEnded(string parameterId)

    implicitHeight: rowLayout.implicitHeight

    QtObject {
        id: prv

        readonly property int spaceS: 4
        readonly property int spaceM: 8
        readonly property int spaceL: 12

        readonly property int labelWidth: 192
        readonly property int controlWidth: 160
        readonly property int numericControlWidth: 160
        readonly property int valueDisplayWidth: 160
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
            Layout.alignment: Qt.AlignVCenter
            text: parameterData ? parameterData.name : ""
            horizontalAlignment: Text.AlignRight
            wrapMode: Text.WordWrap
        }

        // Control loader - loads different controls based on parameter type
        Loader {
            id: controlLoader
            Layout.fillWidth: true
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
                default:
                    return unknownControl
                }
            }
        }
    }

    // Toggle control (checkbox)
    Component {
        id: toggleControl

        CheckBox {
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

        // Wrapper Item needed to properly communicate size to parent Loader via implicitWidth/Height
        Item {
            implicitWidth: dropdown.width
            implicitHeight: dropdown.height

            StyledDropdown {
                id: dropdown
                width: prv.controlWidth

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

        Row {
            spacing: prv.spaceL

            StyledSlider {
                id: slider
                anchors.verticalCenter: parent.verticalCenter
                width: prv.controlWidth

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

            // Display formatted value from plugin (like AU3 does)
            StyledTextLabel {
                anchors.verticalCenter: parent.verticalCenter
                text: parameterData ? parameterData.formattedValue : ""
                horizontalAlignment: Text.AlignLeft
            }
        }
    }

    // Numeric control (text input)
    Component {
        id: numericControl

        RowLayout {
            spacing: prv.spaceM

            IncrementalPropertyControl {
                id: numericInput
                Layout.preferredWidth: prv.numericControlWidth

                // Always use normalized values (0.0 to 1.0) like AU3 does
                currentValue: parameterData ? parameterData.currentValue : 0
                minValue: parameterData ? parameterData.minValue : 0
                maxValue: parameterData ? parameterData.maxValue : 1
                step: parameterData && parameterData.stepSize > 0 ? parameterData.stepSize : 0.01
                decimals: parameterData && parameterData.isInteger ? 0 : 2
                measureUnitsSymbol: ""  // Don't show units here, show in formatted label
                enabled: parameterData ? !parameterData.isReadOnly : false

                // Track editing state for gesture
                property bool isEditing: false

                // Handle focus changes for gesture tracking
                onActiveFocusChanged: {
                    if (activeFocus && !isEditing) {
                        // User focused the control - begin gesture
                        isEditing = true
                        root.gestureStarted(root.parameterId)
                    } else if (!activeFocus && isEditing) {
                        // User left the control - end gesture
                        isEditing = false
                        root.gestureEnded(root.parameterId)
                    }
                }

                onValueEdited: function (newValue) {
                    // If not already editing (e.g., increment/decrement button without focus), start gesture
                    if (!isEditing) {
                        isEditing = true
                        root.gestureStarted(root.parameterId)
                    }

                    root.valueChanged(root.parameterId, newValue);

                    // For button clicks without focus, end gesture immediately
                    if (!activeFocus && isEditing) {
                        isEditing = false
                        root.gestureEnded(root.parameterId)
                    }
                }
            }

            // Display formatted value from plugin (like AU3 does)
            StyledTextLabel {
                Layout.preferredWidth: prv.valueDisplayWidth
                Layout.alignment: Qt.AlignVCenter
                text: parameterData ? parameterData.formattedValue : ""
                horizontalAlignment: Text.AlignLeft
            }
        }
    }

    // Time control (timecode input)
    Component {
        id: timeControl

        Item {
            implicitWidth: timecode.width
            implicitHeight: timecode.height

            Timecode {
                id: timecode

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
        }
    }

    // Read-only control (display only)
    Component {
        id: readonlyControl

        StyledTextLabel {
            text: parameterData ? parameterData.formattedValue : ""
            opacity: 0.7
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
