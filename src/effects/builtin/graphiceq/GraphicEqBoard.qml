import QtQuick 2.15
import QtQuick.Controls 2.15
import Muse.UiComponents 1.0

Item {
    id: board

    property var bandsModel: null

    width: gridLines.width
    height: prv.labelHeight + prv.labelBottomMargin + prv.faderHeight

    QtObject {
        id: prv
        readonly property int minDdBain: -20
        readonly property int maxDdBain: 20
        readonly property int faderHeight: 352
        readonly property int labelHeight: 16
        readonly property int labelBottomMargin: 16
        readonly property int fontSize: 10
    }

    GraphicEqGridLines {
        id: gridLines
        min: prv.minDdBain
        max: prv.maxDdBain
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
                    min: prv.minDdBain
                    max: prv.maxDdBain
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
