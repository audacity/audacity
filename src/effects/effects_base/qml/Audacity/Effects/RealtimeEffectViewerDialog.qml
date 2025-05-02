/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

import Audacity.Effects
import Audacity.Vst

EffectStyledDialogView {
    id: root

    property string instanceId
    property alias effectState: viewerModel.effectState

    property alias viewItem: viewLoader.item

    title: viewerModel.title + " - " + viewerModel.trackName

    contentWidth: viewItem ? Math.max(viewItem.implicitWidth, headerBar.width) : headerBar.width
    contentHeight: 2 * 16 + headerBar.height + (viewItem ? viewItem.implicitHeight : 0)
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
            y: 62
        }
    }

    Component {
        id: builtinViewerComponent
        Column {
            topPadding: 0
            leftPadding: 16
            rightPadding: 16
            bottomPadding: 16

            EffectsViewer {
                id: view
                instanceId: root.instanceId
            }
        }
    }

    ColumnLayout {
        spacing: 0

        RowLayout {
            id: headerBar

            Layout.fillWidth: true
            Layout.margins: 16
            spacing: presetsBar.spacing

            BypassEffectButton {
                id: bypassBtn

                navigation.panel: root.navigationPanel
                navigation.order: 0
                size: presetsBar.implicitHeight
                isMasterEffect: viewerModel.isMasterEffect
                accentButton: viewerModel.isActive

                onClicked: {
                    viewerModel.isActive = !viewerModel.isActive
                }
            }

            EffectPresetsBar {
                id: presetsBar
                navigationPanel: root.navigationPanel
                navigationOrder: 1
                instanceId: root.instanceId
                Layout.fillWidth: true
            }
        }

        Loader {
            id: viewLoader
            Layout.fillWidth: true
        }
    }
}
