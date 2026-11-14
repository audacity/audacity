/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

StyledDialogView {
    id: root

    title: qsTrc("effects", "Missing plugins")

    contentWidth: 560
    contentHeight: 360

    margins: 12

    property var missingPlugins: null

    // Workaround: the dialog is opened during project-page construction,
    // whose async focus transitions (e.g. MainToolBar's onActiveChanged)
    // steal focus from the dialog after it opens. Re-request focus until
    // it sticks. A proper fix would defer opening the dialog until the
    // project page is fully constructed, but that would require a
    // dependency from effects_base on appshell.
    Timer {
        id: refocusTimer
        interval: 100
        repeat: true
        running: root.isOpened
        onTriggered: {
            if (okButton.navigation.active) {
                refocusTimer.stop()
            } else {
                okButton.navigation.requestActive()
            }
        }
    }

    NavigationPanel {
        id: navigationPanel

        name: "MissingPluginsPanel"
        section: root.navigationSection
        order: 0
    }

    ListModel {
        id: pluginsModel
    }

    function populateModel() {
        pluginsModel.clear()
        const entries = root.missingPlugins || []
        for (let i = 0; i < entries.length; ++i) {
            pluginsModel.append({
                name: entries[i].name || "",
                path: entries[i].path || "",
                vendor: entries[i].vendor || ""
            })
        }
    }

    Component.onCompleted: {
        if (!root.missingPlugins) {
            console.error("missing required property `missingPlugins`")
            return
        }
        populateModel()
    }

    ColumnLayout {
        anchors.fill: parent
        spacing: 12

        RowLayout {
            Layout.fillWidth: true
            spacing: 12

            StyledIconLabel {
                Layout.alignment: Qt.AlignTop
                iconCode: IconCode.WARNING
                font.pixelSize: 32
            }

            ColumnLayout {
                Layout.fillWidth: true
                spacing: 8

                StyledTextLabel {
                    Layout.fillWidth: true
                    horizontalAlignment: Text.AlignLeft
                    text: qsTrc("effects", "Missing plugins")
                    font: ui.theme.headerBoldFont
                }

                StyledTextLabel {
                    Layout.fillWidth: true
                    horizontalAlignment: Text.AlignLeft
                    wrapMode: Text.WordWrap
                    text: qsTrc("effects", "Some plugins used in this project were not found. Double-click an entry to see its location.")
                }
            }
        }

        ValueList {
            id: pluginsList

            Layout.fillWidth: true
            Layout.fillHeight: true

            readOnly: true
            sorterEnabled: true

            keyColumnWidth: 100
            keyRoleName: "name"
            keyTitle: qsTrc("effects", "Name")

            valueRoleName: "path"
            valueTitle: qsTrc("effects", "Path")

            navigationSection: root.navigationSection
            navigationOrderStart: 1

            model: pluginsModel

            onHandleItem: function (index, item) {
                detailDialog.pluginName = item.name
                detailDialog.pluginPath = item.path
                detailDialog.pluginVendor = item.vendor
                detailDialog.open()
            }
        }

        FlatButton {
            id: okButton

            Layout.alignment: Qt.AlignRight

            navigation.panel: navigationPanel
            navigation.order: 0
            navigation.name: "OK"

            text: qsTrc("effects", "OK")

            onClicked: {
                root.accept()
            }
        }
    }

    StyledDialogView {
        id: detailDialog

        title: qsTrc("effects", "Plugin details")

        contentWidth: 520
        contentHeight: detailLayout.implicitHeight
        margins: 12

        property string pluginName: ""
        property string pluginPath: ""
        property string pluginVendor: ""

        NavigationPanel {
            id: detailNavPanel
            name: "MissingPluginDetailsPanel"
            section: detailDialog.navigationSection
            order: 0
        }

        Column {
            id: detailLayout

            anchors.fill: parent
            spacing: 12

            StyledTextLabel {
                anchors.left: parent.left
                anchors.right: parent.right

                horizontalAlignment: Text.AlignLeft
                text: detailDialog.pluginName
                font: ui.theme.headerBoldFont
            }

            GridLayout {
                anchors.left: parent.left
                anchors.right: parent.right

                columns: 2
                columnSpacing: 12
                rowSpacing: 6

                StyledTextLabel {
                    Layout.alignment: Qt.AlignRight | Qt.AlignVCenter
                    text: qsTrc("effects", "Vendor:")
                }

                StyledTextLabel {
                    Layout.fillWidth: true
                    horizontalAlignment: Text.AlignLeft
                    text: detailDialog.pluginVendor.length > 0 ? detailDialog.pluginVendor : qsTrc("effects", "Unknown")
                }

                StyledTextLabel {
                    Layout.alignment: Qt.AlignRight | Qt.AlignTop
                    Layout.topMargin: 6
                    text: qsTrc("effects", "Path:")
                }

                TextInputField {
                    id: pathField

                    Layout.fillWidth: true

                    navigation.panel: detailNavPanel
                    navigation.order: 0
                    navigation.name: "PluginPath"

                    readOnly: true
                    currentText: detailDialog.pluginPath

                    Component.onCompleted: {
                        inputField.persistentSelection = true
                    }
                }
            }

            FlatButton {
                id: detailCloseButton

                anchors.right: parent.right

                navigation.panel: detailNavPanel
                navigation.order: 1
                navigation.name: "Close"

                //: Label of the button that closes the dialog
                text: qsTrc("effects", "Close")

                onClicked: detailDialog.close()
            }
        }

        onOpened: {
            Qt.callLater(function () {
                pathField.forceActiveFocus()
                pathField.selectAll()
            })
        }
    }
}
