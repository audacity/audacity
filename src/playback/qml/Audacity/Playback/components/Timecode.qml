/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Layouts 1.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0

import Audacity.Playback 1.0

RowLayout {
    id: root

    property alias value: timecodeModel.value
    property alias mode: timecodeModel.mode

    property alias sampleRate: timecodeModel.sampleRate
    property alias tempo: timecodeModel.tempo
    property alias upperTimeSignature: timecodeModel.upperTimeSignature
    property alias lowerTimeSignature: timecodeModel.lowerTimeSignature

    property alias currentFormat: timecodeModel.currentFormat
    property alias currentFormatStr: timecodeModel.currentFormatStr

    property bool showMenu: true
    property int backgroundLeftRadius: 3

    signal valueChangeRequested(var newValue)

    height: 28

    TimecodeModel {
        id: timecodeModel

        visualItem: root

        onValueChanged: {
            root.valueChangeRequested(value)
        }
    }

    spacing: 1

    RoundedRectangle {
        Layout.preferredWidth: childrenRect.width
        Layout.fillHeight: true

        topLeftRadius: root.backgroundLeftRadius
        bottomLeftRadius: root.backgroundLeftRadius

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
                    model: timecodeModel

                    delegate: TimecodeField {
                        value: symbol

                        isSelected: model.index === timecodeModel.currentEditedFieldIndex
                        isEditable: editable

                        enabled: root.enabled

                        onClicked: {
                            timecodeModel.currentEditedFieldIndex = model.index
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

        menuModel: timecodeModel.availableFormats

        visible: root.showMenu

        onHandleMenuItem: function(itemId) {
            timecodeModel.currentFormat = parseInt(itemId)
        }
    }
}
