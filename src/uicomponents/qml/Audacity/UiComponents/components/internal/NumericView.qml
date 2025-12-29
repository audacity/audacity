/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Layouts 1.15

import Muse.Ui 1.0
import Muse.UiComponents

import Audacity.UiComponents 1.0

RowLayout {
    id: root

    property var model: null

    property bool showMenu: true
    property int backgroundLeftRadius: 3
    property Border border: Border {}
    property int arrowSpacing: 1
    property color textColor: ui.theme.fontSecondaryColor
    property color backgroundColor: ui.theme.backgroundQuarternaryColor

    signal valueChangeRequested(var newValue)

    height: 28

    spacing: root.arrowSpacing

    Component.onCompleted: {
        root.model.visualItem = root
    }

    Connections {
        target: root.model
        function onValueChanged() {
            root.valueChangeRequested(value)
        }
    }

    RoundedRectangle {
        Layout.preferredWidth: childrenRect.width
        Layout.fillHeight: true

        topLeftRadius: root.backgroundLeftRadius
        bottomLeftRadius: root.backgroundLeftRadius

        border: root.border
        color: root.backgroundColor

        Item {
            property int margin: 6

            width: row.width + margin * 2
            height: parent.height

            Row {
                id: row

                anchors.left: parent.left
                anchors.leftMargin: parent.margin
                anchors.top: parent.top
                anchors.bottom: parent.bottom

                spacing: 0

                Repeater {
                    model: root.model

                    delegate: NumericField {
                        height: row.height
                        value: symbol

                        isSelected: model.index === root.model.currentEditedFieldIndex
                        isEditable: editable

                        color: root.textColor
                        enabled: root.enabled

                        onClicked: {
                            root.model.currentEditedFieldIndex = model.index
                        }
                    }
                }
            }
        }
    }

    ArrowMenuButton {
        id: menuBtn

        Layout.preferredWidth: 16
        Layout.fillHeight: true

        menuModel: root.model.availableFormats

        border: root.border
        backgroundColor: root.backgroundColor
        iconColor: root.textColor
        visible: root.showMenu

        onHandleMenuItem: function(itemId) {
            root.model.currentFormat = parseInt(itemId)
        }
    }
}
