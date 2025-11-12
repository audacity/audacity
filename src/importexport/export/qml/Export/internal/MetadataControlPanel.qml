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

    property var metadataModel: null
    property var tagView: null

    property NavigationPanel navigationPanel: null
    property int navigationOrder: 0

    property int templateButtonWidth: 105
    property int tagButtonWidth: 70

    spacing: 6

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

        Layout.preferredWidth: root.templateButtonWidth

        text: qsTrc("export", "Load template")

        navigation.panel: root.navigationPanel
        navigation.column: root.navigationOrder

        onClicked: {
            metadataModel.loadTemplate()
        }
    }

    FlatButton {
        id: saveBtn

        Layout.preferredWidth: root.templateButtonWidth

        text: qsTrc("export", "Save template")

        navigation.panel: root.navigationPanel
        navigation.column: loadBtn.navigation.column + 1

        onClicked: {
            metadataModel.saveTemplate()
        }
    }

    FlatButton {
        id: setDefaultBtn

        Layout.preferredWidth: root.templateButtonWidth

        text: qsTrc("export", "Set as default")

        navigation.panel: root.navigationPanel
        navigation.column: saveBtn.navigation.column + 1

        onClicked: {
            metadataModel.setAsDefault()
        }
    }

    SeparatorLine {}

    FlatButton {
        id: deleteBtn

        Layout.preferredWidth: tagButtonWidth

        enabled: !metadataModel.isStandardTag(tagView.currentSourceRow)

        text: qsTrc("export", "Delete")

        navigation.panel: root.navigationPanel
        navigation.column: setDefaultBtn.navigation.column + 1

        onClicked: {
            metadataModel.deleteTag(tagView.currentSourceRow)
        }
    }

    FlatButton {
        id: addBtn

        Layout.preferredWidth: tagButtonWidth

        text: qsTrc("export", "Add tag")

        navigation.panel: root.navigationPanel
        navigation.column: deleteBtn.navigation.column + 1

        onClicked: {
            metadataModel.addTag()
        }
    }
}
