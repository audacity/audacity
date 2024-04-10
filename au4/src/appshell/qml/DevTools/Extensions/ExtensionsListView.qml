import QtQuick 2.15

import Muse.UiComponents 1.0
import Muse.Extensions 1.0

Rectangle {
    color: ui.theme.backgroundSecondaryColor

    DevExtensionsListModel {
        id: devModel
    }

    StyledListView {
        anchors.fill: parent

        model: devModel.extensionsList()

        delegate: ListItemBlank {
            anchors.left: parent ? parent.left : undefined
            anchors.right: parent ? parent.right : undefined
            height: 96

            StyledTextLabel {
                anchors.left: parent.left
                anchors.right: parent.right
                anchors.leftMargin: 8
                anchors.rightMargin: 8
                anchors.verticalCenter: parent.verticalCenter
                horizontalAlignment: Text.AlignLeft
                text: (model.index + 1) + ": " + modelData.title
                + "\n uri: " + modelData.uri
                + "\n type: " + modelData.type
            }

            onClicked: devModel.clicked(modelData.uri)
        }
    }
}
