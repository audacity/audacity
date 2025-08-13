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

        property var pressedFader: null
        property var lastPaintingFader: null
        property bool isPainting: false

        function mouseIsOver(item) {
            const p = mouseArea.mapToItem(item, mouseArea.mouseX, mouseArea.mouseY)
            return p.x >= 0 && p.x <= item.width && p.y >= 0 && p.y <= item.height
        }

        function faderUnderMouse() {
            for (let i = 0; i < repeater.count; ++i) {
                const f = repeater.itemAt(i).getFader()
                if (prv.mouseIsOver(f)) {
                    return f
                }
            }
            return null
        }

        function updateFader(fader) {
            if (!fader) {
                return
            }

            const p = mouseArea.mapToItem(fader, mouseArea.mouseX, mouseArea.mouseY)
            const newValue = fader.min + (fader.max - fader.min) * (1 - p.y / fader.height)
            fader.requestNewValue(Math.min(Math.max(newValue, fader.min), fader.max))
        }
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

    MouseArea {
        id: mouseArea

        anchors.fill: parent

        hoverEnabled: true

        onPressed: function(mouse) {
            if (mouse.button !== Qt.LeftButton) {
                return
            }
            prv.isPainting = false
            prv.lastPaintingFader = null
            const fader = prv.faderUnderMouse()
            prv.pressedFader = fader
            prv.updateFader(fader)
            if (fader) {
                fader.showTooltip()
            }
        }

        onPositionChanged: function(mouse) {
            if (!(mouse.buttons & Qt.LeftButton)) {
                return
            }

            const fader = prv.faderUnderMouse()

            if (!prv.isPainting) {
                prv.isPainting = fader && fader !== prv.pressedFader
                if (prv.isPainting) {
                    prv.pressedFader.hideTooltip()
                }
            }


            if (!prv.isPainting) {
                prv.updateFader(prv.pressedFader)
            } else {
                if (fader) {
                    prv.lastPaintingFader = fader
                }
                const targetFader = fader || prv.lastPaintingFader
                prv.updateFader(targetFader)
            }
        }

        onDoubleClicked: function(mouse) {
            if (mouse.button !== Qt.LeftButton) {
                return
            }
            const fader = prv.faderUnderMouse()
            if (fader) {
                fader.requestNewValue(0)
            }
        }
    }

    Row {
        id: faderRow

        spacing: 16
        x: gridLines.gridlineHorizontalCenter - width / 2
        height: prv.faderHeight + prv.labelHeight + prv.labelBottomMargin

        Repeater {
            id: repeater

            model: bandsModel

            delegate: Column {
                width: fader.width
                spacing: prv.labelBottomMargin

                function getFader() {
                    return fader
                }

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
                    anchors.horizontalCenter: parent.horizontalCenter

                    value: model.dbGain

                    function requestNewValue(newValue) {
                        model.dbGain = newValue
                    }
                }
            }
        }
    }
}
