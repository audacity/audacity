import QtQuick 2.15
import QtQuick.Controls 2.15
import Muse.UiComponents 1.0

Item {
    id: root

    property var bandsModel: null
    property int minDbGain: -1
    property int maxDbGain: -1

    width: gridLines.width
    height: prv.labelHeight + prv.labelBottomMargin + prv.faderHeight

    QtObject {
        id: prv

        readonly property int faderHeight: 352
        readonly property int labelHeight: 16
        readonly property int labelBottomMargin: 16
        readonly property int fontSize: 10
    }

    GraphicEqGridLines {
        id: gridLines

        min: root.minDbGain
        max: root.maxDbGain
        lineWidth: faderRow.width + faderRow.spacing
        height: prv.faderHeight
        anchors.horizontalCenter: parent.horizontalCenter
        fontSize: prv.fontSize
        y: faderRow.y + prv.labelHeight + prv.labelBottomMargin
    }

    Row {
        id: faderRow

        spacing: 16
        x: gridLines.gridlineHorizontalCenter - width / 2
        height: prv.faderHeight + prv.labelHeight + prv.labelBottomMargin

        Repeater {
            model: bandsModel

            delegate: Column {
                width: fader.width
                spacing: prv.labelBottomMargin

                StyledTextLabel {
                    width: 16
                    height: prv.labelHeight
                    font.pixelSize: prv.fontSize
                    horizontalAlignment: Text.AlignHCenter
                    verticalAlignment: Text.AlignVCenter
                    text: model.centerFreq
                    anchors.horizontalCenter: parent.horizontalCenter
                    elide: Text.ElideNone
                }

                GraphicEqFader {
                    id: fader

                    height: prv.faderHeight
                    min: root.minDbGain
                    max: root.maxDbGain
                    value: model.dbGain
                    anchors.horizontalCenter: parent.horizontalCenter
                    onNewValueRequested: function(newValue) {
                        model.dbGain = newValue;
                    }
                }
            }
        }
    }
}
