/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

import Audacity.Effects
import Audacity.BuiltinEffects
import Audacity.Lv2
import Audacity.Vst
import Audacity.AudioUnit

EffectStyledDialogView {
    id: root

    property string instanceId
    property alias effectState: viewerModel.effectState

    title: viewerModel.title + " - " + viewerModel.trackName
    navigationSection.name: title

    contentWidth: Math.max(viewLoader.width, prv.minimumWidth)
    contentHeight: 2 * prv.padding + viewLoader.height + presetsBar.height

    alwaysOnTop: true

    QtObject {
        id: prv
        property int minimumWidth: viewerModel.effectFamily === EffectFamily.LV2 ? 500 : 270
        property int padding: viewerModel.effectFamily == EffectFamily.Builtin ? ui.theme.extra.space_16 : ui.theme.extra.space_4
        property alias viewItem: viewLoader.item
    }

    Component.onCompleted: {
        viewerModel.load()
        switch (viewerModel.effectFamily) {
        case EffectFamily.Builtin:
            viewLoader.sourceComponent = builtinViewerComponent
            break
        case EffectFamily.AudioUnit:
            viewLoader.sourceComponent = audioUnitViewerComponent
            break
        case EffectFamily.LV2:
            viewLoader.sourceComponent = lv2ViewerComponent
            break
        case EffectFamily.VST3:
            viewLoader.sourceComponent = vstViewerComponent
            break
        default:
            viewLoader.sourceComponent = null
        }
    }

    RealtimeEffectViewerDialogModel {
        id: viewerModel
        effectState: root.effectState
        dialogView: root
        navigationPanel: root.navigationPanel
    }

    Component {
        id: audioUnitViewerComponent
        AudioUnitViewer {
            id: view
            instanceId: root.instanceId
            topPadding: headerBar.y + headerBar.height + prv.padding
            minimumWidth: prv.minimumWidth
        }
    }

    Component {
        id: lv2ViewerComponent
        Lv2Viewer {
            id: view
            instanceId: root.instanceId
            effectState: root.effectState
            title: root.title
        }
    }

    Component {
        id: vstViewerComponent
        VstViewer {
            id: view
            instanceId: root.instanceId
            topPadding: headerBar.y + headerBar.height + prv.padding
            minimumWidth: prv.minimumWidth
        }
    }

    Component {
        id: builtinViewerComponent
        Column {
            topPadding: ui.theme.extra.space_0
            leftPadding: prv.padding
            rightPadding: prv.padding
            bottomPadding: prv.padding

            BuiltinEffectViewer {
                id: view
                instanceId: root.instanceId
                usedDestructively: false
            }
        }
    }

    ColumnLayout {
        spacing: ui.theme.extra.space_0
        anchors.fill: parent

        WindowContainer {
            Layout.fillWidth: true

            window: Window {
                id: win

                color: ui.theme.backgroundPrimaryColor

                height: headerBar.implicitHeight + prv.padding * 2
                width: headerBar.implicitWidth

                RowLayout {
                    id: headerBar
                    anchors.fill: parent
                    anchors.margins: prv.padding
                    spacing: presetsBar.spacing

                    BypassEffectButton {
                        id: bypassBtn

                        navigation.panel: root.navigationPanel
                        navigation.order: 0
                        navigation.name: "Bypass effect"
                        size: presetsBar.implicitHeight
                        isMasterEffect: viewerModel.isMasterEffect
                        accentButton: viewerModel.isActive

                        onClicked: viewerModel.isActive = !viewerModel.isActive
                    }

                    EffectPresetsBar {
                        id: presetsBar

                        parentWindow: root.window
                        navigationPanel: root.navigationPanel
                        navigationOrder: 1
                        instanceId: root.instanceId
                        Layout.fillWidth: true
                    }
                }
            }
        }
        Loader {
            id: viewLoader
        }
    }
}
