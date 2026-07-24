/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick
import QtQuick.Window

import Muse.Ui
import Muse.UiComponents

import Audacity.UiComponents

BaseSection {
    id: root

    property var preferencesModel: null
    property var plugins: []

    signal ensureContentVisibleRequested(var contentRect)

    function ensurePluginVisible(plugin) {
        const item = plugin.firstPreference || plugin
        const position = item.mapToItem(root.parent, 0, 0)
        ensureContentVisibleRequested(Qt.rect(position.x, position.y, item.width, Math.min(item.height, 120)))
    }

    function ensureActiveFocusVisible(item) {
        const window = item.Window.window
        const focusedItem = window ? window.activeFocusItem : null
        if (focusedItem) {
            const position = focusedItem.mapToItem(root.parent, 0, 0)
            ensureContentVisibleRequested(Qt.rect(position.x, position.y, focusedItem.width, focusedItem.height))
        }
    }

    title: qsTrc("preferences", "Audacity plug-in preferences")

    navigation.enabled: false
    navigationOrderEnd: navigation.order + Math.max(plugins.length - 1, 0)

    Column {
        width: parent.width
        spacing: 24

        Repeater {
            model: root.plugins

            delegate: Column {
                id: pluginCard

                width: parent.width
                spacing: 12

                property int pluginIndex: index
                property var pluginData: modelData
                property Item firstPreference: preferencesRepeater.itemAt(0)
                property NavigationPanel navigationPanel: NavigationPanel {
                    name: pluginCard.pluginData.name
                    accessible.name: pluginCard.pluginData.name
                    enabled: pluginCard.enabled && pluginCard.visible
                    direction: NavigationPanel.Vertical
                    section: root.navigation.section
                    order: root.navigation.order + pluginCard.pluginIndex

                    onActiveChanged: function (active) {
                        if (active) {
                            Qt.callLater(function () {
                                if (pluginCard.navigationPanel.active && pluginCard.navigationPanel.highlight) {
                                    root.ensurePluginVisible(pluginCard)
                                }
                            })
                        }
                    }

                    onNavigationEvent: function (event) {
                        if (event.type === NavigationEvent.Up || event.type === NavigationEvent.Down) {
                            Qt.callLater(function () {
                                root.ensureActiveFocusVisible(pluginCard)
                            })
                        }
                    }
                }

                Column {
                    width: parent.width
                    spacing: 2

                    StyledTextLabel {
                        width: parent.width
                        horizontalAlignment: Text.AlignLeft
                        font: ui.theme.bodyBoldFont
                        text: pluginCard.pluginData.name
                    }

                    StyledTextLabel {
                        width: parent.width
                        horizontalAlignment: Text.AlignLeft
                        opacity: 0.7
                        text: pluginCard.pluginData.version ? qsTrc("preferences", "%1, %2").arg(pluginCard.pluginData.vendor).arg(pluginCard.pluginData.version) : pluginCard.pluginData.vendor
                    }
                }

                Repeater {
                    id: preferencesRepeater

                    model: pluginCard.pluginData.preferences

                    delegate: Column {
                        id: preferenceRow

                        width: parent.width
                        spacing: 5

                        property int preferenceIndex: index
                        property var preferenceData: modelData
                        property var currentValue: preferenceData.value
                        readonly property int navigationOrder: preferenceIndex * 2
                        readonly property bool isBoolean: preferenceData.type === "bool"
                        readonly property bool isInteger: preferenceData.type === "int64"
                        readonly property bool isDouble: preferenceData.type === "double"
                        readonly property bool isString: preferenceData.type === "string"
                        readonly property bool isEnumeration: preferenceData.type === "enum"
                        readonly property bool isFile: preferenceData.type === "file"
                        readonly property bool isDirectory: preferenceData.type === "directory"
                        readonly property bool isPath: isFile || isDirectory

                        StyledTextLabel {
                            width: parent.width
                            horizontalAlignment: Text.AlignLeft
                            text: preferenceRow.preferenceData.unit ? qsTrc("preferences", "%1 (%2)").arg(preferenceRow.preferenceData.name).arg(preferenceRow.preferenceData.unit) : preferenceRow.preferenceData.name
                        }

                        StyledTextLabel {
                            width: parent.width
                            visible: !isEmpty
                            horizontalAlignment: Text.AlignLeft
                            wrapMode: Text.WordWrap
                            opacity: 0.7
                            text: preferenceRow.preferenceData.description
                        }

                        FilePicker {
                            width: parent.width
                            visible: preferenceRow.isPath

                            pickerType: preferenceRow.isDirectory ? FilePicker.PickerType.Directory : FilePicker.PickerType.File
                            dialogTitle: qsTrc("preferences", "Choose %1").arg(preferenceRow.preferenceData.name)
                            pathFieldTitle: preferenceRow.preferenceData.name
                            path: preferenceRow.currentValue
                            dir: preferenceRow.isDirectory ? preferenceRow.currentValue : ""

                            navigation: pluginCard.navigationPanel
                            navigationRowOrderStart: preferenceRow.navigationOrder

                            onPathEdited: function (newPath) {
                                preferenceRow.currentValue = newPath
                                root.preferencesModel.setAudacityPluginPreference(pluginCard.pluginData.id, preferenceRow.preferenceData.key, newPath)
                            }
                        }

                        CheckBox {
                            width: parent.width
                            visible: preferenceRow.isBoolean

                            checked: preferenceRow.currentValue

                            navigation.panel: pluginCard.navigationPanel
                            navigation.order: preferenceRow.navigationOrder
                            navigation.accessible.name: preferenceRow.preferenceData.name

                            onClicked: {
                                preferenceRow.currentValue = !checked
                                root.preferencesModel.setAudacityPluginPreference(pluginCard.pluginData.id, preferenceRow.preferenceData.key, preferenceRow.currentValue)
                            }
                        }

                        StyledDropdown {
                            width: Math.min(parent.width, 360)
                            visible: preferenceRow.isEnumeration

                            model: preferenceRow.preferenceData.choices
                            textRole: "text"
                            valueRole: "value"
                            currentIndex: indexOfValue(preferenceRow.currentValue)

                            navigation.panel: pluginCard.navigationPanel
                            navigation.order: preferenceRow.navigationOrder
                            navigation.accessible.name: qsTrc("preferences", "%1: %2").arg(preferenceRow.preferenceData.name).arg(currentText)

                            onActivated: function (newIndex, newValue) {
                                preferenceRow.currentValue = newValue
                                root.preferencesModel.setAudacityPluginPreference(pluginCard.pluginData.id, preferenceRow.preferenceData.key, newValue)
                            }
                        }

                        TextInputField {
                            width: Math.min(parent.width, 360)
                            visible: preferenceRow.isInteger || preferenceRow.isDouble || preferenceRow.isString

                            currentText: preferenceRow.currentValue
                            navigation.panel: pluginCard.navigationPanel
                            navigation.order: preferenceRow.navigationOrder
                            navigation.accessible.name: qsTrc("preferences", "%1: %2").arg(preferenceRow.preferenceData.name).arg(currentText)

                            onTextEditingFinished: function (newTextValue) {
                                preferenceRow.currentValue = newTextValue
                                root.preferencesModel.setAudacityPluginPreference(pluginCard.pluginData.id, preferenceRow.preferenceData.key, newTextValue)
                            }
                        }

                        StyledTextLabel {
                            width: parent.width
                            visible: preferenceRow.preferenceData.hasMinimum || preferenceRow.preferenceData.hasMaximum
                            horizontalAlignment: Text.AlignLeft
                            opacity: 0.7
                            text: {
                                if (preferenceRow.preferenceData.hasMinimum && preferenceRow.preferenceData.hasMaximum) {
                                    return qsTrc("preferences", "Allowed range: %1 to %2").arg(preferenceRow.preferenceData.minimum).arg(preferenceRow.preferenceData.maximum)
                                }
                                if (preferenceRow.preferenceData.hasMinimum) {
                                    return qsTrc("preferences", "Minimum: %1").arg(preferenceRow.preferenceData.minimum)
                                }
                                return qsTrc("preferences", "Maximum: %1").arg(preferenceRow.preferenceData.maximum)
                            }
                        }
                    }
                }

                SeparatorLine {
                    width: parent.width
                    visible: pluginCard.pluginIndex + 1 < root.plugins.length
                }
            }
        }
    }
}
