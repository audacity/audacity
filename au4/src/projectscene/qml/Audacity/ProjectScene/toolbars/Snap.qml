/*
* Audacity: A Digital Audio Editor
*/
import QtQuick

import Muse.UiComponents

import Audacity.ProjectScene

Row {
    id: root

    property bool isSnapEnabled: false

    property string currentSnapMode: ""
    property var contextMenuModel: null

    spacing: 6

    signal snapEnableChangeRequested(var snap)
    signal handleMenuItem(var itemId)

    StyledTextLabel {
        anchors.verticalCenter: parent.verticalCenter

        text: qsTrc("projectscene", "Snap")
    }

    CheckBox {
        id: snapCheckBox

        anchors.verticalCenter: parent.verticalCenter

        checked: root.isSnapEnabled

        onClicked: function() {
            root.snapEnableChangeRequested(!snapCheckBox.checked)
        }
    }

    SnapChooseItem {
        width: 144
        height: root.height

        text: root.currentSnapMode

        contextMenuModel: root.contextMenuModel

        onHandleMenuItem: function(itemId) {
            root.handleMenuItem(itemId)
        }
    }
}
