import QtQuick
import QtQuick.Layouts
import QtQuick.Controls

import Muse.Ui
import Muse.UiComponents

import Audacity.ProjectScene

Rectangle {
    id: root

    property var model: null
    property var context: null

    width: 32
    color: ui.theme.backgroundQuarternaryColor

    Rectangle {
        id: leftBorder

        width: 1
        height: parent.height
        color: ui.theme.strokeColor
        opacity: 0.1
    }

    StyledListView {
        id: verticalRulersListView

        anchors.fill: parent

        clip: false

        ScrollBar.vertical: null

        //! NOTE Sync with TracksClipsView
        TracksViewStateModel {
            id: tracksViewState
            onTracksVericalYChanged: {
                verticalRulersListView.contentY = tracksViewState.tracksVericalY
            }
        }

        Component.onCompleted: {
            tracksViewState.init()
        }

        header: Rectangle {
            height: 2
            width: parent.width
            color: "transparent"
        }

        footer: Item {
            height: tracksViewState.tracksVerticalScrollPadding
        }

        property real lockedVerticalScrollPosition
        property bool verticalScrollLocked: tracksViewState.tracksVerticalScrollLocked

        onVerticalScrollLockedChanged: {
            lockedVerticalScrollPosition = contentY
        }

        onContentYChanged: {
            if (verticalScrollLocked) {
                verticalRulersListView.contentY = lockedVerticalScrollPosition
            } else {
                tracksViewState.changeTracksVericalY(verticalRulersListView.contentY)
            }
        }

        interactive: false

        model: root.model
        delegate: Rectangle {

            width: root.width
            height: trackViewState.trackHeight

            color: ui.theme.backgroundQuarternaryColor

            z: model.isTrackFocused ? 10 : 0

            Component.onCompleted: {
                trackViewState.init()
                clipsModel.init()
            }

            TracksViewStateModel {
                id: trackViewState
                trackId: model.trackId
            }

            ClipsListModel {
                id: clipsModel
                context: root.context
                trackId: model.trackId
            }

            Rectangle {
                id: leftBorder

                anchors.left: parent.left
                anchors.top: parent.top

                width: 1
                height: parent.height
                color: ui.theme.strokeColor
                opacity: 0.1
            }

            Rectangle {
                id: header

                anchors.top: parent.top
                anchors.left: parent.left
                anchors.right: parent.right

                width: parent.width
                height: trackViewState.isTrackCollapsed ? 1 : 20
                color: "#000000"
                opacity: 0.20
                visible: !trackViewState.isTrackCollapsed
            }

            Rectangle {
                id: topBorder

                anchors.top: parent.top
                anchors.left: header.left
                anchors.right: header.right
                anchors.topMargin: -border.width

                height: border.width

                visible: model.isTrackFocused

                color: "transparent"

                border.color: "#7EB1FF"
                border.width: 2
            }

            Rectangle {
                id: bottomBorder

                anchors.bottom: parent.bottom
                anchors.left: header.left
                anchors.right: header.right

                height: 2

                color: "#7EB1FF"

                visible: model.isTrackFocused
            }

            SeparatorLine {
                id: sep

                color: "#000000"
                opacity: 0.20
                anchors.bottom: parent.bottom
                thickness: 2
            }

            Item {
                id: ruler

                TrackRulerModel {
                    id: rulerModel

                    isStereo: clipsModel.isStereo
                    height: ruler.height
                    isCollapsed: trackViewState.isTrackCollapsed
                }

                anchors.top: header.bottom
                anchors.bottom: sep.top
                anchors.bottomMargin: 1
                anchors.left: leftBorder.right
                anchors.right: parent.right

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
                            color: ui.theme.strokeColor
                        }

                        Rectangle {
                            id: tickExtension

                            anchors.left: tick.right
                            anchors.verticalCenter: parent.verticalCenter

                            width: parent.width - tick.width
                            height: 1

                            color: "#475157"

                            visible: modelData.fullWidthTick
                        }

                        Item {
                            anchors.right: parent.right
                            anchors.rightMargin: 4

                            anchors.verticalCenter: modelData.alignment == 0 ? parent.verticalCenter : undefined
                            anchors.bottom: modelData.alignment == 1 ? parent.bottom : undefined
                            anchors.bottomMargin: modelData.alignment == 1 ? 1 : undefined
                            anchors.top: modelData.alignment == -1 ? parent.top : undefined

                            height: aLabelMetrics.ascent
                            width: parent.width - tick.width - 4

                            Text {
                                id: aLabel

                                anchors.right: parent.right
                                anchors.verticalCenter: parent.verticalCenter

                                text: Math.abs(modelData.value).toFixed(1)
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
                                anchors.rightMargin: 1
                                anchors.verticalCenter: aLabel.verticalCenter

                                width: 3
                                height: 1
                                color: "#F9F9FA"

                                visible: modelData.value < 0
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
                            color: "#A9B0BD"
                            anchors.verticalCenter: parent.verticalCenter
                        }
                    }
                }
            }
        }
    }
}
