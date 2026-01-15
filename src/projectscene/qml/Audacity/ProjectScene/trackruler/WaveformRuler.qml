import QtQuick
import Muse.Ui
import Muse.UiComponents
import Audacity.ProjectScene

Item {
    id: root

    required property bool isCollapsed
    required property real channelHeightRatio

    Component.onCompleted: {
        rulerModel.init()
    }

    TrackRulerModel {
        id: rulerModel

        height: root.height

        trackId: model.trackId
        isStereo: model.isStereo

        isCollapsed: root.isCollapsed
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

        availableRulerTypes: rulerModel.availableRulerTypes
        rulerType: rulerModel.rulerType
        isDefaultZoom: rulerModel.isDefaultZoom
        isMaxZoom: rulerModel.isMaxZoom
        isMinZoom: rulerModel.isMinZoom
        isHalfWave: rulerModel.isHalfWave

        placementPolicies: PopupView.PreferLeft

        onRulerTypeChangeRequested: function (rulerType) {
            rulerModel.rulerType = rulerType
        }

        onZoomInRequested: {
            rulerModel.zoomIn()
        }

        onZoomOutRequested: {
            rulerModel.zoomOut()
        }

        onZoomResetRequested: {
            rulerModel.resetZoom()
        }

        onToggleHalfWaveRequested: {
            rulerModel.toggleHalfWave()
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

                width: 8
                height: 1
                color: ui.theme.fontSecondaryColor

                antialiasing: true
            }

            Rectangle {
                id: tickExtension

                anchors.left: tick.right
                anchors.verticalCenter: parent.verticalCenter

                width: parent.width - tick.width
                height: 1

                color: ui.theme.extra["waveform_ruler_tick_extension_color"]

                visible: modelData.fullWidthTick

                antialiasing: true
            }

            Item {
                anchors.top: modelData.alignment == -1 ? parent.top : undefined
                anchors.bottom: modelData.alignment == 1 ? parent.bottom : undefined
                anchors.right: parent.right

                anchors.rightMargin: 20
                anchors.bottomMargin: modelData.alignment == 1 ? 1 : undefined
                anchors.verticalCenter: modelData.alignment == 0 ? parent.verticalCenter : undefined

                height: aLabelMetrics.ascent
                width: parent.width - tick.width - 4

                Text {
                    id: aLabel

                    anchors.right: parent.right
                    anchors.verticalCenter: parent.verticalCenter

                    text: rulerModel.sampleToText(modelData.value)
                    color: ui.theme.extra["waveform_ruler_label_color"]
                    font.pixelSize: 11

                    font.bold: modelData.bold

                    FontMetrics {
                        id: aLabelMetrics
                        font: aLabel.font
                    }
                }

                Rectangle {
                    id: minusIndicator

                    anchors.right: aLabel.left
                    anchors.rightMargin: 2
                    anchors.verticalCenter: aLabel.verticalCenter

                    width: 3
                    height: 1
                    color: ui.theme.fontSecondaryColor

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
                width: 4
                height: 1
                color: ui.theme.extra["waveform_ruler_small_step_color"]
                anchors.verticalCenter: parent.verticalCenter
                antialiasing: true
            }
        }
    }
}
