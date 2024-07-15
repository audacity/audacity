/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Layouts 1.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0

import Audacity.Playback 1.0

import "../components"

RowLayout {
    id: root

    property alias value: bpmModel.value

    signal valueChangeRequested(var newValue)

    height: 28

    BPMModel {
        id: bpmModel

        visualItem: root

        onValueChanged: {
            root.valueChangeRequested(value)
        }
    }

    spacing: 1

    RoundedRectangle {
        Layout.preferredWidth: childrenRect.width
        Layout.preferredHeight: root.height

        topLeftRadius: 3
        bottomLeftRadius: 3

        color: ui.theme.backgroundQuarternaryColor

        Item {
            property int margin: 6

            width: row.width + margin * 2
            height: row.height

            Row {
                id: row

                anchors.left: parent.left
                anchors.leftMargin: parent.margin
                anchors.verticalCenter: parent.verticalCenter

                spacing: 0

                Repeater {
                    model: bpmModel

                    delegate: TimecodeField {
                        value: symbol

                        isSelected: model.index === bpmModel.currentEditedFieldIndex
                        isEditable: editable

                        onClicked: {
                            bpmModel.currentEditedFieldIndex = model.index
                        }
                    }
                }
            }
        }
    }

    ColumnLayout {
        Layout.preferredWidth: 16
        Layout.preferredHeight: root.height

        spacing: 1

        ArrowButton {
            Layout.fillWidth: true
            Layout.fillHeight: true

            isDown: false
            bottomRightRadius: 0

            onClicked: function(mouse) {
                bpmModel.upValue()
            }
        }

        ArrowButton {
            Layout.fillWidth: true
            Layout.fillHeight: true
            Layout.alignment: Qt.AlignBottom

            isDown: true
            topRightRadius: 0

            onClicked: function(mouse) {
                bpmModel.downValue()
            }
        }
    }
}
