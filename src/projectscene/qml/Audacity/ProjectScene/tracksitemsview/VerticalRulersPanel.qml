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

    visible: model.isVerticalRulersVisible

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

        //! NOTE Sync with TracksItemsView
        TracksViewStateModel {
            id: tracksViewState
            onTracksVerticalOffsetChanged: {
                verticalRulersListView.contentY = tracksViewState.tracksVerticalOffset
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
                tracksViewState.changeTracksVerticalOffset(verticalRulersListView.contentY)
            }
        }

        interactive: false

        model: root.model
        delegate: Rectangle {

            width: root.width
            height: trackViewState.trackHeight

            color: ui.theme.backgroundQuarternaryColor

            Component.onCompleted: {
                trackViewState.init()
                rulerModel.init()
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
                isVerticalRulersVisible: root.model.isVerticalRulersVisible
                rulerType: model.trackRulerType
                availableRulerTypes: model.availableRulerTypes

                placementPolicies: PopupView.PreferLeft

                onHideRulersRequested: {
                    root.model.toggleVerticalRuler();
                }

                onRulerTypeChangeRequested: function(rulerType) {
                    root.model.setTrackRulerType(model.trackId, rulerType);
                }
            }

            TrackViewStateModel {
                id: trackViewState
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
                height: trackViewState.isTrackCollapsed ? 0 : 20
                color: "#000000"
                opacity: 0.20
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

                    isStereo: model.isStereo
                    height: ruler.height
                    isCollapsed: trackViewState.isTrackCollapsed
                    channelHeightRatio: trackViewState.channelHeightRatio
                    rulerType: model.trackRulerType
                }

                anchors.top: header.bottom
                anchors.bottom: sep.top
                anchors.left: leftBorder.right
                anchors.right: parent.right

                anchors.bottomMargin: 1

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

                            anchors.rightMargin: 3
                            anchors.bottomMargin: modelData.alignment == 1 ? 1 : undefined
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
                                anchors.rightMargin: 2
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
                            color: ui.theme.isDark ? "#868B8E": "#8B8C96"
                            anchors.verticalCenter: parent.verticalCenter
                            antialiasing: true
                        }
                    }
                }
            }
        }
    }
}
