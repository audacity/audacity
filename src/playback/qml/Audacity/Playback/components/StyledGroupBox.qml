/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15
import QtQuick.Layouts 1.15
import QtQuick.Controls 2.15

import Muse.UiComponents 1.0
import Muse.Ui 1.0

Item {
    id: root

    property string title: ""

    property int titleSpacing: 12
    property int itemSpacing: 8
    property int itemMargin: 12

    property int borderWidth: 1
    property int boarderRadius: 2

    property int value

    property bool enabled: true

    property color backgroundColor: ui.theme.backgroundPrimaryColor

    property var model: null

    signal valueChangeRequested(int value)

    ColumnLayout {
        anchors.fill: parent

        spacing: root.titleSpacing

        StyledTextLabel {
            Layout.fillWidth: true

            text: root.title
            horizontalAlignment: Text.AlignLeft
        }
        
        Rectangle {
            Layout.fillWidth: true
            Layout.fillHeight: true

            color: root.backgroundColor
            border.width: root.borderWidth
            border.color: ui.theme.strokeColor
            radius: root.boarderRadius

            Item {
                anchors.fill: parent
                anchors.margins: root.itemMargin

                RadioButtonGroup {
                    id: meterStyleGroup

                    orientation: Qt.Vertical
                    width: parent.width

                    spacing: root.itemSpacing

                    model: root.model

                    delegate: RoundedRadioButton {
                        id: meterStyleButton
                        text: modelData["label"]
                        checked: root.value == modelData["value"]
                        enabled: root.enabled

                        onToggled: {
                            root.valueChangeRequested(modelData["value"])
                        }
                    }
                }
            }
        }
    }
}