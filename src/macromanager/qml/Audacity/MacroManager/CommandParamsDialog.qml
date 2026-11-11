/*
* Audacity: A Digital Audio Editor
*/
pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

import Audacity.MacroManager

StyledDialogView {
    id: root

    property string command: ""
    property var params: ({})

    title: paramsModel.title

    contentWidth: prv.contentWidth
    margins: prv.margins

    QtObject {
        id: prv

        readonly property int contentWidth: 360
        readonly property int margins: 16
        readonly property int rowsSpacing: 12
        readonly property int itemSpacing: 8
        readonly property int fieldWidth: 160
        readonly property int buttonsTopMargin: 16
        readonly property int buttonWidth: 64
        readonly property int buttonHeight: 28
        readonly property int buttonsNavigationOrder: 1000

        function confirm() {
            root.ret = {
                errcode: 0,
                value: paramsModel.params()
            }
            root.hide()
        }
    }

    CommandParamsModel {
        id: paramsModel
    }

    Component.onCompleted: {
        paramsModel.load(root.command, root.params)
    }

    ColumnLayout {
        id: content

        width: parent.width

        spacing: 0

        ColumnLayout {
            Layout.fillWidth: true

            spacing: prv.rowsSpacing

            Repeater {
                model: paramsModel

                delegate: RowLayout {
                    id: row

                    required property int index
                    required property string key
                    required property string description
                    required property int type
                    required property var min
                    required property var max
                    required property var value
                    required property var options
                    required property bool optional
                    required property bool included

                    property NavigationPanel navigationPanel: NavigationPanel {
                        name: "CommandParam" + row.index
                        direction: NavigationPanel.Horizontal
                        section: root.navigationSection
                        order: row.index + 1
                        accessible.name: row.description.length > 0 ? row.description : row.key
                    }

                    Layout.fillWidth: true

                    spacing: prv.itemSpacing

                    Item {
                        implicitWidth: includedCheckBox.implicitWidth
                        implicitHeight: includedCheckBox.implicitHeight

                        CheckBox {
                            id: includedCheckBox

                            visible: row.optional
                            checked: row.included

                            navigation.panel: row.navigationPanel
                            navigation.order: 0
                            navigation.enabled: visible
                            navigation.accessible.name: row.description.length > 0 ? row.description : row.key

                            onClicked: {
                                paramsModel.setIncluded(row.index, !checked)
                            }
                        }
                    }

                    StyledTextLabel {
                        Layout.fillWidth: true

                        text: row.description.length > 0 ? row.description : row.key
                        horizontalAlignment: Qt.AlignLeft
                    }

                    Loader {
                        Layout.preferredWidth: prv.fieldWidth

                        enabled: row.included

                        sourceComponent: {
                            switch (row.type) {
                            case CommandParamsModel.Integer:
                            case CommandParamsModel.Float:
                                return numberField
                            case CommandParamsModel.Boolean:
                                return booleanField
                            case CommandParamsModel.Enum:
                                return enumField
                            default:
                                return textField
                            }
                        }
                    }

                    Component {
                        id: textField

                        TextInputField {
                            currentText: row.value

                            navigation.panel: row.navigationPanel
                            navigation.order: 1

                            onTextChanged: function (newTextValue) {
                                paramsModel.setValue(row.index, newTextValue)
                            }
                        }
                    }

                    Component {
                        id: numberField

                        IncrementalPropertyControl {
                            currentValue: row.value
                            minValue: row.min !== undefined && row.min !== null ? row.min : -Number.MAX_VALUE
                            maxValue: row.max !== undefined && row.max !== null ? row.max : Number.MAX_VALUE
                            decimals: row.type === CommandParamsModel.Float ? 2 : 0
                            step: 1

                            navigation.panel: row.navigationPanel
                            navigation.order: 1

                            onValueEdited: function (newValue) {
                                paramsModel.setValue(row.index, newValue)
                            }
                        }
                    }

                    Component {
                        id: booleanField

                        CheckBox {
                            checked: row.value

                            navigation.panel: row.navigationPanel
                            navigation.order: 1

                            onClicked: {
                                paramsModel.setValue(row.index, !checked)
                            }
                        }
                    }

                    Component {
                        id: enumField

                        StyledDropdown {
                            id: dropdown

                            model: row.options
                            currentIndex: dropdown.indexOfValue(row.value)

                            navigation.panel: row.navigationPanel
                            navigation.order: 1

                            onActivated: function (index, value) {
                                paramsModel.setValue(row.index, value)
                            }
                        }
                    }
                }
            }
        }

        ButtonBox {
            id: buttons

            Layout.fillWidth: true
            Layout.topMargin: prv.buttonsTopMargin

            navigationPanel.section: root.navigationSection
            navigationPanel.order: prv.buttonsNavigationOrder

            FlatButton {
                Layout.preferredWidth: prv.buttonWidth
                Layout.preferredHeight: prv.buttonHeight

                text: qsTrc("global", "Cancel")
                buttonRole: ButtonBoxModel.RejectRole
                buttonId: ButtonBoxModel.Cancel

                onClicked: {
                    root.reject()
                }
            }

            FlatButton {
                Layout.preferredWidth: prv.buttonWidth
                Layout.preferredHeight: prv.buttonHeight

                text: qsTrc("macromanager", "Confirm")
                buttonRole: ButtonBoxModel.AcceptRole
                buttonId: ButtonBoxModel.CustomButton + 1
                accentButton: true

                onClicked: {
                    prv.confirm()
                }
            }
        }
    }
}
