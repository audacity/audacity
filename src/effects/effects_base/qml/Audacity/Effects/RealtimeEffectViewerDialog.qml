/*
* Audacity: A Digital Audio Editor
*/
import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

import Audacity.Effects

StyledDialogView {
    id: root

    property alias type: viewer.type
    property alias instanceId: viewer.instanceId
    property alias effectState: viewer.effectState

    title: viewer.title + " - " + viewerModel.trackName

    contentWidth: viewer.implicitWidth
    contentHeight: layout.implicitHeight + 16
    alwaysOnTop: true
    modal: true
    margins: 16

    Component.onCompleted: {
        viewerModel.load()
    }

    RealtimeEffectViewerDialogModel {
        id: viewerModel
        effectState: root.effectState
        onTrackRemoved: root.close()
    }

    ColumnLayout {
        id: layout

        anchors.fill: parent

        RowLayout {
            id: headerBar

            Layout.fillWidth: true
            Layout.preferredHeight: 40

            spacing: 8

            FlatButton {
                id: powerButton

                Layout.margins: 0
                Layout.alignment: Qt.AlignLeft | Qt.AlignVCenter
                Layout.preferredWidth: headerBar.height

                icon: IconCode.BYPASS
                iconFont: ui.theme.toolbarIconsFont
                accentButton: true

                onClicked: {
                    accentButton = !accentButton
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
