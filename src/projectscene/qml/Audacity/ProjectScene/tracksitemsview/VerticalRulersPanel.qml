import QtQuick
import QtQuick.Layouts
import QtQuick.Controls

import Muse.Ui
import Muse.UiComponents

import Audacity.ProjectScene

Rectangle {
    id: root

    property ViewTracksListModel model: null
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

        delegate: Loader {
            width: root.width
            height: trackViewState.trackHeight

            sourceComponent: model.trackType !== TrackType.LABEL ? waveComp : emptyComp

            TrackViewStateModel {
                id: trackViewState
                trackId: model.trackId
            }

            Component.onCompleted: {
                trackViewState.init()
            }

            Component {
                id: emptyComp

                Item {}
            }

            Component {
                id: waveComp

                Rectangle {
                    color: ui.theme.backgroundQuarternaryColor

                    MouseArea {
                        id: mouseClickBlocker // to prevent clicks from reaching and modifying the viewport
                        anchors.fill: parent
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

                    ColumnLayout {

                        anchors.top: header.bottom
                        anchors.bottom: sep.top
                        anchors.left: leftBorder.right
                        anchors.right: parent.right
                        anchors.bottomMargin: 1

                        WaveformRuler {
                            Layout.fillWidth: true
                            Layout.fillHeight: true

                            visible: model.isWaveformViewVisible

                            isCollapsed: trackViewState.isTrackCollapsed
                            channelHeightRatio: trackViewState.channelHeightRatio

                            onSetTrackRulerTypeRequested: function (rulerType) {
                                root.model.setTrackRulerType(model.trackId, rulerType)
                            }
                        }

                        SpectrogramRuler {
                            Layout.fillWidth: true
                            Layout.fillHeight: true

                            visible: model.isSpectrogramViewVisible
                        }
                    }
                }
            }
        }
    }
}
