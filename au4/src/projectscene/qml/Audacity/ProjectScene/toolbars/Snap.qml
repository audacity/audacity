/*
* Audacity: A Digital Audio Editor
*/
import QtQuick
import QtQuick.Layouts

import Muse.UiComponents

import Audacity.ProjectScene

RowLayout {
    id: root

    property bool isSnapEnabled: false

    property string currentSnapMode: ""
    property var contextMenuModel: null

    spacing: 6

    signal snapEnableChangeRequested(var snap)
    signal handleMenuItem(var itemId)

    StyledTextLabel {
        Layout.alignment: Qt.AlignVCenter

        text: qsTrc("projectscene", "Snap")
    }

    CheckBox {
        id: snapCheckBox

        Layout.alignment: Qt.AlignVCenter

        checked: root.isSnapEnabled

        onClicked: function() {
            root.snapEnableChangeRequested(!snapCheckBox.checked)
        }
    }

    SnapChooseItem {
        Layout.fillWidth: true
        Layout.fillHeight: true

        text: root.currentSnapMode

        contextMenuModel: root.contextMenuModel

        onHandleMenuItem: function(itemId) {
            root.handleMenuItem(itemId)
        }
    }
}
