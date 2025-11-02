/*
* Audacity: A Digital Audio Editor
*/
import QtQuick
import QtQuick.Controls
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

import Audacity.Export 1.0

RowLayout {
    id: root

    Layout.fillHeight: true
    Layout.fillWidth: true

    Layout.leftMargin: 16
    Layout.rightMargin: 12
    Layout.topMargin: 10
    Layout.bottomMargin: 10

    spacing: 6

    property var metadataModel: null
    property var tagView: null

    property NavigationPanel navigationPanel: null
    property int navigationOrder: 0

    StyledTextLabel {
        id: title

        text: qsTrc("export", "Metadata editor")

        font.pixelSize: 18
    }

    Item {
        // spacer
        Layout.fillHeight: true
        Layout.fillWidth: true
    }

    FlatButton {
        id: loadBtn

        Layout.preferredWidth: 106

        text: qsTrc("export", "Load template")

        navigation.panel: root.navigationPanel
        navigation.column: root.navigationOrder

        onClicked: {
            metadataModel.loadTemplate()
        }
    }

    FlatButton {
        id: saveBtn

        Layout.preferredWidth: 105

        text: qsTrc("export", "Save template")

        navigation.panel: root.navigationPanel
        navigation.column: loadBtn.navigation.column + 1

        onClicked: {
            metadataModel.saveTemplate()
        }
    }

    FlatButton {
        id: setDefaultBtn

        Layout.preferredWidth: 103

        text: qsTrc("export", "Set as default")

        navigation.panel: root.navigationPanel
        navigation.column: saveBtn.navigation.column + 1

        onClicked: {
            metadataModel.setAsDefault()
        }
    }

    SeparatorLine {
        orientation: Qt.Vertical
    }

    FlatButton {
        id: deleteBtn

        Layout.preferredWidth: 61

        enabled: !metadataModel.isStandardTag(tagView.currentIndex)

        text: qsTrc("export", "Delete")

        navigation.panel: root.navigationPanel
        navigation.column: setDefaultBtn.navigation.column + 1

        onClicked: {
            metadataModel.deleteTag(tagView.currentIndex)
        }
    }

    FlatButton {
        id: addBtn

        Layout.preferredWidth: 69

        text: qsTrc("export", "Add tag")

        navigation.panel: root.navigationPanel
        navigation.column: deleteBtn.navigation.column + 1

        onClicked: {
            metadataModel.addTag()

            // highlight right away
            tagView.currentIndex = metadataModel.rowCount() - 1
        }
    }
}
