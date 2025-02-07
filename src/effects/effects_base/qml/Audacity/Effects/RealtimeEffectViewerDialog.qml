/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

import Audacity.Effects

StyledDialogViewWithoutNavigationSection {
    id: root

    property alias type: viewer.type
    property alias instanceId: viewer.instanceId
    property alias effectState: viewer.effectState

    title: viewer.title + " - " + viewerModel.trackName

    contentWidth: viewer.implicitWidth
    contentHeight: layout.implicitHeight + 16
    alwaysOnTop: true
    margins: 16

    Component.onCompleted: {
        viewerModel.load()
    }

    RealtimeEffectViewerDialogModel {
        id: viewerModel
        effectState: root.effectState
    }

    ColumnLayout {
        id: layout

        anchors.fill: parent

        RowLayout {
            id: headerBar

            Layout.fillWidth: true
            Layout.preferredHeight: 40

            spacing: 8

            BypassEffectButton {
                Layout.margins: 0
                Layout.alignment: Qt.AlignLeft | Qt.AlignVCenter
                Layout.preferredWidth: headerBar.height

                isMasterEffect: viewerModel.isMasterEffect
                accentButton: viewerModel.isActive

                onClicked: {
                    viewerModel.isActive = !viewerModel.isActive
                }
            }

            FlatButton {
                id: manageBtn

                Layout.alignment: Qt.AlignLeft | Qt.AlignVCenter
                Layout.preferredWidth: implicitWidth

                text: qsTrc("effects", "Presets & settings")
                buttonRole: ButtonBoxModel.CustomRole
                buttonId: ButtonBoxModel.CustomButton + 1
                onClicked: viewer.manage(manageBtn)
            }
        }

        EffectsViewer {
            id: viewer
            width: parent.width
        }
    }

}
