/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

import Audacity.ProjectScene

import "audio"

//! What a track sends to busses besides its output: a level each, before or after the track's volume and pan
StyledPopupView {
    id: root

    property var trackId: null

    contentWidth: 320
    contentHeight: content.implicitHeight

    onOpened: {
        sendsModel.load()
        Qt.callLater(function () {
            root.repositionWindowIfNeed()
        })
        navPanel.requestActive()
    }

    TrackSendsModel {
        id: sendsModel
        trackId: root.trackId
    }

    NavigationPanel {
        id: navPanel
        name: "SendsPopup"
        enabled: root.isOpened
        direction: NavigationPanel.Vertical
        section: root.navigationSection
    }

    ColumnLayout {
        id: content

        width: root.contentWidth
        spacing: 8

        StyledTextLabel {
            Layout.fillWidth: true
            horizontalAlignment: Text.AlignLeft
            //: What a track sends to busses besides its output
            text: qsTrc("projectscene", "Sends")
            font: ui.theme.bodyBoldFont
        }

        StyledTextLabel {
            Layout.fillWidth: true
            horizontalAlignment: Text.AlignLeft
            visible: sendsList.count === 0
            text: qsTrc("projectscene", "This track sends to no bus besides its output.")
        }

        Repeater {
            id: sendsList

            model: sendsModel

            RowLayout {
                Layout.fillWidth: true
                spacing: 6

                StyledTextLabel {
                    Layout.preferredWidth: 72
                    horizontalAlignment: Text.AlignLeft
                    text: model.title
                }

                VolumeSlider {
                    value: model.level

                    navigation.panel: navPanel
                    navigation.order: index * 3
                    //: Accessibility name of the level slider of a send to a bus
                    navigation.accessible.name: qsTrc("projectscene", "Send level to %1").arg(model.title)

                    onNewVolumeRequested: function (newValue, completed) {
                        sendsModel.setLevel(index, newValue, completed)
                    }
                }

                FlatButton {
                    Layout.preferredWidth: 36
                    Layout.preferredHeight: 24

                    //: Short for pre-fader: a send taken before the track's volume and pan
                    text: qsTrc("projectscene", "Pre")
                    toolTipTitle: qsTrc("projectscene", "Pre-fader")
                    toolTipDescription: qsTrc("projectscene", "Sends before this track's volume and pan, rather than after")
                    accentButton: model.preFader

                    navigation.panel: navPanel
                    navigation.order: index * 3 + 1

                    onClicked: {
                        sendsModel.setPreFader(index, !model.preFader)
                    }
                }

                FlatButton {
                    Layout.preferredWidth: 24
                    Layout.preferredHeight: 24

                    icon: IconCode.CLOSE_X_ROUNDED
                    toolTipTitle: qsTrc("projectscene", "Remove send")

                    navigation.panel: navPanel
                    navigation.order: index * 3 + 2

                    onClicked: {
                        sendsModel.removeSend(index)
                    }
                }
            }
        }

        FlatButton {
            Layout.fillWidth: true
            Layout.preferredHeight: 24

            icon: IconCode.PLUS
            orientation: Qt.Horizontal
            text: qsTrc("projectscene", "Add send")

            navigation.panel: navPanel
            navigation.order: sendsList.count * 3

            onClicked: {
                addMenuLoader.toggleOpened(sendsModel.addMenu())
            }

            StyledMenuLoader {
                id: addMenuLoader

                onHandleMenuItem: function (itemId) {
                    sendsModel.addSend(itemId)
                }
            }
        }
    }
}
