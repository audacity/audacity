import QtQuick
import Muse.Ui
import Muse.UiComponents
import Audacity.ProjectScene

Item {
    id: root

    required property bool isStereo
    required property bool isCollapsed
    required property int rulerType
    required property real channelHeightRatio
    required property bool isVerticalRulersVisible

    signal hideVerticalRulerRequested
    signal setTrackRulerTypeRequested(int rulerType)

    Component.onCompleted: {
        rulerModel.init()
    }

    TrackRulerModel {
        id: rulerModel

        height: root.height

        isStereo: root.isStereo
        isCollapsed: root.isCollapsed
        rulerType: root.rulerType
        channelHeightRatio: root.channelHeightRatio
    }

    MouseArea {
        anchors.fill: parent
        acceptedButtons: Qt.RightButton | Qt.LeftButton

        onClicked: {
            customisePopup.toggleOpened()
        }
    }

    TrackRulerCustomizePopup {
        id: customisePopup
        isVerticalRulersVisible: root.isVerticalRulersVisible
        rulerType: model.trackRulerType
        availableRulerTypes: model.availableRulerTypes

        placementPolicies: PopupView.PreferLeft

        onHideRulersRequested: {
            root.hideVerticalRulerRequested()
        }

        onRulerTypeChangeRequested: function (rulerType) {
            root.setTrackRulerTypeRequested(rulerType)
        }
    }

    Repeater {
        id: fullStepsRepeater

        model: rulerModel.fullSteps

        delegate: Item {
            required property var modelData

            y: modelData.y
            width: parent.width
            height: 1

            Rectangle {
                id: tick

                anchors.left: parent.left
                anchors.verticalCenter: parent.verticalCenter

                width: 7
                height: 1
                color: ui.theme.isDark ? "#F4F7F9" : "#F4F5F9"

                antialiasing: true
            }

            Rectangle {
                id: tickExtension

                anchors.left: tick.right
                anchors.verticalCenter: parent.verticalCenter

                width: parent.width - tick.width
                height: 1

                color: "#475157"

                visible: modelData.fullWidthTick

                antialiasing: true
            }

            Item {
                anchors.top: modelData.alignment == -1 ? parent.top : undefined
                anchors.bottom: modelData.alignment == 1 ? parent.bottom : undefined
                anchors.right: parent.right

                anchors.rightMargin: ui.theme.extra.space_3
                anchors.bottomMargin: modelData.alignment == 1 ? ui.theme.extra.space_1 : undefined
                anchors.verticalCenter: modelData.alignment == 0 ? parent.verticalCenter : undefined

                height: aLabelMetrics.ascent
                width: parent.width - tick.width - 4

                Text {
                    id: aLabel

                    anchors.right: parent.right
                    anchors.verticalCenter: parent.verticalCenter

                    text: rulerModel.sampleToText(modelData.value)
                    color: "#F9F9FA"
                    font.pixelSize: 10

                    font.bold: modelData.bold

                    FontMetrics {
                        id: aLabelMetrics
                        font: aLabel.font
                    }
                }

                Rectangle {
                    id: minusIndicator

                    anchors.right: aLabel.left
                    anchors.rightMargin: ui.theme.extra.space_2
                    anchors.verticalCenter: aLabel.verticalCenter

                    width: 3
                    height: 1
                    color: "#F4F5F9"

                    visible: modelData.value < 0

                    antialiasing: true
                }
            }
        }
    }

    Repeater {
        id: smallStepsRepeater

        model: rulerModel.smallSteps

        delegate: Item {
            required property var modelData

            y: modelData.y
            width: parent.width
            height: 1

            Rectangle {
                width: 3
                height: 1
                color: ui.theme.isDark ? "#868B8E" : "#8B8C96"
                anchors.verticalCenter: parent.verticalCenter
                antialiasing: true
            }
        }
    }
}
