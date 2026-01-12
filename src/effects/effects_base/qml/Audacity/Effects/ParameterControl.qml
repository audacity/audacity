/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

import Audacity.Effects

Item {
    id: root

    property var parameterData: null

    signal valueChanged(double value)

    implicitHeight: controlLoader.height

    QtObject {
        id: prv

        readonly property int spaceS: 4
        readonly property int spaceM: 8
        readonly property int spaceL: 12

        readonly property int labelWidth: 120
        readonly property int controlWidth: 144
        readonly property int numericControlWidth: 200
        readonly property int valueDisplayWidth: 80
    }

    RowLayout {
        anchors.fill: parent
        anchors.rightMargin: prv.spaceL
        spacing: prv.spaceL

        // Parameter label
        StyledTextLabel {
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
            checked: {
                if (!parameterData) {
                    return false
                }
                var midpoint = (parameterData.minValue + parameterData.maxValue) / 2
                return parameterData.currentValue > midpoint
            }
            enabled: parameterData ? !parameterData.isReadOnly : false

            onClicked: {
                // Send the opposite of current value (toggle)
                var midpoint = (parameterData.minValue + parameterData.maxValue) / 2
                var isCurrentlyOn = parameterData.currentValue > midpoint
                root.valueChanged(isCurrentlyOn ? parameterData.minValue : parameterData.maxValue)
            }
        }
    }

    // Dropdown control (combobox)
    Component {
        id: dropdownControl

        Item {
            implicitWidth: dropdown.width
            implicitHeight: dropdown.height

            StyledDropdown {
                id: dropdown
                width: prv.controlWidth

                currentIndex: {
                    if (!parameterData || !parameterData.enumIndices) {
                        return 0
                    }

                    // Find the index that matches the current value
                    for (var i = 0; i < parameterData.enumIndices.length; i++) {
                        if (Math.abs(parameterData.enumIndices[i] - parameterData.currentValue) < 0.001) {
                            return i
                        }
                    }
                    return 0
                }

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
                    if (parameterData && parameterData.enumIndices && index >= 0 && index < parameterData.enumIndices.length) {
                        root.valueChanged(parameterData.enumIndices[index])
                    }
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
                anchors.verticalCenter: parent.verticalCenter
                width: prv.controlWidth

                from: parameterData ? parameterData.minValue : 0
                to: parameterData ? parameterData.maxValue : 1
                value: parameterData ? parameterData.currentValue : 0
                stepSize: parameterData && parameterData.stepSize > 0 ? parameterData.stepSize : 0.01
                enabled: parameterData ? !parameterData.isReadOnly : false

                onMoved: {
                    root.valueChanged(value)
                }
            }

            // IncrementalPropertyControl {
            //     width: 60
            //
            //     // Always use normalized values (0.0 to 1.0) like AU3 does
            //     currentValue: parameterData ? parameterData.currentValue : 0
            //     minValue: parameterData ? parameterData.minValue : 0
            //     maxValue: parameterData ? parameterData.maxValue : 1
            //     step: parameterData && parameterData.stepSize > 0 ? parameterData.stepSize : 0.01
            //     decimals: parameterData && parameterData.isInteger ? 0 : 2
            //     measureUnitsSymbol: ""  // Don't show units here, show in formatted label
            //     enabled: parameterData ? !parameterData.isReadOnly : false
            //
            //     onValueEdited: function(newValue) {
            //         root.valueChanged(newValue)
            //     }
            // }

            // Display formatted value from plugin (like AU3 does)
            StyledTextLabel {
                anchors.verticalCenter: parent.verticalCenter
                text: {
                    if (!parameterData)
                        return ""
                    var displayText = parameterData.currentValueString || ""
                    // Add units if available and not already in the formatted string
                    if (parameterData.units && displayText.indexOf(parameterData.units) === -1) {
                        displayText = displayText + " " + parameterData.units
                    }
                    return displayText
                }
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
                Layout.preferredWidth: prv.numericControlWidth

                // Always use normalized values (0.0 to 1.0) like AU3 does
                currentValue: parameterData ? parameterData.currentValue : 0
                minValue: parameterData ? parameterData.minValue : 0
                maxValue: parameterData ? parameterData.maxValue : 1
                step: parameterData && parameterData.stepSize > 0 ? parameterData.stepSize : 0.01
                decimals: parameterData && parameterData.isInteger ? 0 : 2
                measureUnitsSymbol: ""  // Don't show units here, show in formatted label
                enabled: parameterData ? !parameterData.isReadOnly : false

                onValueEdited: function (newValue) {
                    root.valueChanged(newValue)
                }
            }

            // Display formatted value from plugin (like AU3 does)
            StyledTextLabel {
                Layout.preferredWidth: prv.valueDisplayWidth
                Layout.alignment: Qt.AlignVCenter
                text: {
                    if (!parameterData)
                        return ""
                    var displayText = parameterData.currentValueString || ""
                    // Add units if available and not already in the formatted string
                    if (parameterData.units && displayText.indexOf(parameterData.units) === -1) {
                        displayText = displayText + " " + parameterData.units
                    }
                    return displayText
                }
                horizontalAlignment: Text.AlignLeft
            }
        }
    }

    // Read-only control (display only)
    Component {
        id: readonlyControl

        StyledTextLabel {
            text: {
                if (!parameterData) {
                    return ""
                }
                // Use formatted string from plugin if available, otherwise show normalized value
                if (parameterData.currentValueString && parameterData.currentValueString.length > 0) {
                    return parameterData.currentValueString
                }
                // Fallback: show normalized value with units
                var valueStr = parameterData.currentValue.toFixed(2)
                if (parameterData.units) {
                    return valueStr + " " + parameterData.units
                }
                return valueStr
            }
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
