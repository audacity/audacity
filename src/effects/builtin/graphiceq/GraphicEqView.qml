import QtQuick
import QtQuick.Controls
import Muse.UiComponents
import Audacity.Effects

import "../common"

EffectBase {
    id: root

    property string title: qsTrc("effects/graphiceq", "Graphic EQ")
    property bool isApplyAllowed: true

    width: bandsRow.width
    implicitHeight: 428

    model: graphicEq

    GraphicEqViewModel {
        id: graphicEq

        instanceId: root.instanceId
    }

    Column {
        spacing: 16
        Row {
            id: bandsRow

            spacing: 8

            Repeater {
                model: graphicEq.bandsModel

                Column {
                    spacing: 16
                    width: 32
                    padding: 4

                    Text {
                        anchors.horizontalCenter: parent.horizontalCenter
                        height: 16

                        text: model.centerFreq
                        horizontalAlignment: Text.AlignHCenter
                        verticalAlignment: Text.AlignVCenter
                        font.pixelSize: 12
                        elide: Text.ElideNone
                        wrapMode: Text.NoWrap
                        clip: false
                    }

                    Slider {
                        id: slider
                        anchors.horizontalCenter: parent.horizontalCenter
                        width: parent.width
                        height: 352

                        orientation: Qt.Vertical
                        from: -20
                        to: 20
                        value: model.dbGain
                        onValueChanged: {
                            model.dbGain = value
                        }
                    }
                }
            }
        }

        Row {
            spacing: 8
            anchors.horizontalCenter: parent.horizontalCenter

            FlatButton {
                text: qsTrc("effects/graphiceq", "Flatten")
                width: 64
                height: 28
                onClicked: graphicEq.flatten()
            }

            FlatButton {
                id: cancelBtn
                text: qsTrc("effects/graphiceq", "Invert")
                width: 64
                height: 28
                onClicked: graphicEq.invert()
            }
        }
    }

    Component.onCompleted: {
        graphicEq.init()
    }
}
