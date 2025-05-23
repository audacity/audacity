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

    title: viewerModel.title + " - " + viewerModel.trackName
    navigationSection.name: title

    implicitWidth: prv.viewItem ? Math.max(prv.viewItem.implicitWidth, headerBar.width) : headerBar.width
    implicitHeight: 2 * prv.padding + headerBar.height + (prv.viewItem ? prv.viewItem.implicitHeight : 0)
    minimumWidth: 270

    alwaysOnTop: true

    QtObject {
        id: prv
        property int padding: viewerModel.isVst3() ? 4 : 16
        property alias viewItem: viewLoader.item
    }

    Component.onCompleted: {
        viewerModel.load()
        viewLoader.sourceComponent = viewerModel.isVst3() ? vstViewerComponent : builtinViewerComponent
    }

    RealtimeEffectViewerDialogModel {
        id: viewerModel
        effectState: root.effectState
        dialogView: root
        navigationPanel: root.navigationPanel
    }

    Component {
        id: vstViewerComponent
        VstViewer {
            id: view
            instanceId: root.instanceId
            topPadding: headerBar.y + headerBar.height + prv.padding
            minimumWidth: root.minimumWidth
        }
    }

    Component {
        id: builtinViewerComponent
        Column {
            topPadding: 0
            leftPadding: prv.padding
            rightPadding: prv.padding
            bottomPadding: prv.padding

            EffectsViewer {
                id: view
                instanceId: root.instanceId
            }
        }
    }

    ColumnLayout {
        spacing: 0
        anchors.fill: parent

        RowLayout {
            id: headerBar

            Layout.fillWidth: true
            Layout.margins: prv.padding
            spacing: presetsBar.spacing

            BypassEffectButton {
                id: bypassBtn

                navigation.panel: root.navigationPanel
                navigation.order: 0
                navigation.name: "Bypass effect"
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
