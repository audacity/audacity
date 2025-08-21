import QtQuick
import QtQuick.Controls
import Muse.Ui
import Muse.UiComponents
import Audacity.Effects

import "../common"

BuiltinEffectBase {
    id: root

    property string title: qsTrc("effects/graphiceq", "Graphic EQ")
    property bool isApplyAllowed: true

    width: boardRectangle.width
    implicitHeight: boardRectangle.height

    model: graphicEq

    GraphicEqViewModel {
        id: graphicEq
    }

    Rectangle {
        id: boardRectangle

        width: boardAndButtons.width
        height: boardAndButtons.height
        anchors.centerIn: parent

        radius: 8
        color: ui.theme.backgroundSecondaryColor
        border.color: ui.theme.strokeColor

        Column {
            id: boardAndButtons

            padding: 16
            spacing: 16
            width: board.width + 2 * padding

            GraphicEqBoard {
                id: board
                anchors.horizontalCenter: parent.horizontalCenter
                bandsModel: graphicEq.bandsModel
                minDbGain: graphicEq.minDbGain
                maxDbGain: graphicEq.maxDbGain
            }

            Row {
                id: buttons

                spacing: 8
                anchors.horizontalCenter: parent.horizontalCenter

                FlatButton {
                    text: qsTrc("effects/graphiceq", "Flatten")
                    width: 64
                    height: 28
                    onClicked: graphicEq.bandsModel.flatten()
                }

                FlatButton {
                    text: qsTrc("effects/graphiceq", "Invert")
                    width: 64
                    height: 28
                    onClicked: graphicEq.bandsModel.invert()
                }
            }
        }
    }

    Component.onCompleted: {
        graphicEq.init()
    }
}
