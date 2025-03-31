/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick

import Muse.Ui
import Muse.UiComponents

import Audacity.Effects
import Audacity.Vst

StyledDialogViewWithoutNavigationSection {
    id: root

    property string instanceId
    property alias effectState: viewerModel.effectState

    property alias viewItem: viewLoader.item

    title: viewerModel.title + " - " + viewerModel.trackName

    contentWidth: viewItem ? Math.max(viewItem.implicitWidth, headerBar.width) : headerBar.width
    contentHeight: viewItem ? headerBar.height + viewItem.implicitHeight : headerBar.height
    alwaysOnTop: true

    Component.onCompleted: {
        viewerModel.load()
        viewLoader.sourceComponent = viewerModel.isVst3() ? vstViewerComponent : builtinViewerComponent
    }

    RealtimeEffectViewerDialogModel {
        id: viewerModel
        effectState: root.effectState
    }

    Component {
        id: vstViewerComponent
        VstViewer {
            id: view
            instanceId: root.instanceId
            x: (root.width - view.width) / 2
            y: headerBar.height
        }
    }

    Component {
        id: builtinViewerComponent
        Row {
            padding: 8
            EffectsViewer {
                id: view
                instanceId: root.instanceId
                x: (root.width - view.width) / 2
                y: headerBar.height
            }
        }
    }

    Row {
        id: headerBar

        padding: 8
        spacing: 8

        BypassEffectButton {
            id: bypassBtn

            size: 36
            isMasterEffect: viewerModel.isMasterEffect
            accentButton: viewerModel.isActive

            onClicked: {
                viewerModel.isActive = !viewerModel.isActive
            }
        }

        FlatButton {
            id: manageBtn

            width: implicitWidth
            height: bypassBtn.size

            text: qsTrc("effects", "Presets & settings")
            buttonRole: ButtonBoxModel.CustomRole
            buttonId: ButtonBoxModel.CustomButton + 1
            onClicked: viewItem.manage(manageBtn)
        }
    }

    Loader {
        id: viewLoader
    }
}
