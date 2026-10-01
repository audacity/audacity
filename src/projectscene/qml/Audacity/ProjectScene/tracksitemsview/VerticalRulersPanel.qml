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

    property var navPanels: null

    width: 32
    color: ui.theme.backgroundQuarternaryColor

    visible: model.isVerticalRulersVisible

    TracksViewStateModel {
        id: tracksViewState
    }

    Component.onCompleted: {
        tracksViewState.init()
    }

    Rectangle {
        id: leftBorder

        width: 1
        height: parent.height
        color: ui.theme.strokeColor
        opacity: 0.1
    }

    TracksListView {
        id: verticalRulersListView

        anchors.fill: parent

        clip: false

        tracksViewState: tracksViewState

        model: root.model

        delegate: Loader {
            id: rulerLoader

            property int index: model.index

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
                    id: rulerItem

                    color: ui.theme.backgroundQuarternaryColor

                    NavigationControl {
                        id: navCtrl

                        name: "VerticalRuler"
                        enabled: root.enabled && root.visible
                        panel: root.navPanels && root.navPanels[rulerLoader.index] ? root.navPanels[rulerLoader.index] : null
                        order: 0

                        accessible.role: MUAccessible.Information
                        accessible.name: qsTrc("projectscene", "Track %1: %2, vertical ruler").arg(rulerLoader.index + 1).arg(model.trackTitle)

                        onActiveChanged: function (active) {
                            if (active) {
                                rulerItem.forceActiveFocus()
                                verticalRulersListView.ensureVerticallyVisible(rulerLoader)
                            }
                        }
                    }

                    NavigationFocusBorder {
                        navigationCtrl: navCtrl
                        drawOutsideParent: false
                        border.color: ui.theme.fontSecondaryColor
                    }

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
                        color: ui.theme.extra["black_color"]
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

                        border.color: ui.theme.extra["focus_state_color"]
                        border.width: 2
                    }

                    Rectangle {
                        id: bottomBorder

                        anchors.bottom: parent.bottom
                        anchors.left: header.left
                        anchors.right: header.right

                        height: 2

                        color: ui.theme.extra["focus_state_color"]

                        visible: model.isTrackFocused
                    }

                    SeparatorLine {
                        id: sep

                        color: ui.theme.extra["black_color"]
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

                        Loader {
                            id: waveformRulerLoader

                            Layout.fillWidth: true
                            Layout.fillHeight: true

                            active: false
                            visible: model.isWaveformViewVisible

                            // Defer ruler creation, to speed up track loading
                            Component.onCompleted: Qt.callLater(function () {
                                waveformRulerLoader.active = Qt.binding(function () {
                                    return model.isWaveformViewVisible
                                })
                            })

                            sourceComponent: WaveformRuler {
                                isCollapsed: trackViewState.isTrackCollapsed
                                channelHeightRatio: trackViewState.channelHeightRatio
                            }
                        }

                        SeparatorLine {
                            color: ui.theme.extra["waveform_ruler_tick_extension_color"]
                        }

                        Loader {
                            id: spectrogramRulerLoader

                            Layout.fillWidth: true
                            Layout.fillHeight: true

                            active: false
                            visible: model.isSpectrogramViewVisible

                            // Defer ruler creation, to speed up track loading
                            Component.onCompleted: Qt.callLater(function () {
                                spectrogramRulerLoader.active = Qt.binding(function () {
                                    return model.isSpectrogramViewVisible
                                })
                            })

                            sourceComponent: SpectrogramTrackRulers {
                                trackId: model.trackId
                                isStereo: model.isStereo
                                channelHeightRatio: trackViewState.channelHeightRatio
                            }
                        }
                    }
                }
            }
        }
    }
}
