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
import Audacity.AudioUnit
import Audacity.Vst

EffectStyledDialogView {
    id: root

    property alias effectState: viewerModel.effectState

    title: viewerModel.title + " - " + viewerModel.trackName
    navigationSection.name: title

    contentWidth: Math.max(viewerLoader.width + prv.viewMargins * 2, prv.minimumWidth)
    contentHeight: {
        let height = 0
        height += prv.showTopPanel ? topPanel.height : prv.viewMargins
        height += viewerLoader.height
        height += prv.showBottomPanel ? bottomPanel.height : prv.viewMargins
        return height
    }

    QtObject {
        id: prv
        property alias viewer: viewerLoader.item

        property int minimumWidth: viewerModel.effectFamily === EffectFamily.LV2 ? 500 : 250
        property int panelMargins: (viewerModel.effectFamily === EffectFamily.Builtin || viewerModel.viewerComponentType === ViewerComponentType.Generated) ? 16 : 4
        property int viewMargins: (viewerModel.effectFamily === EffectFamily.Builtin || viewerModel.viewerComponentType === ViewerComponentType.Generated) ? 16 : 0
        property int separatorHeight: (viewerModel.effectFamily === EffectFamily.Builtin || viewerModel.viewerComponentType === ViewerComponentType.Generated) ? separator.height + prv.panelMargins : 0
        property bool showTopPanel: true
        property bool showBottomPanel: false
    }

    Component.onCompleted: {
        viewerModel.load()
        loadViewer()
    }

    // Listen to UI mode changes from the presets bar menu
    Connections {
        target: presetsBar.presetsBarModel
        function onUseVendorUIChanged() {
            viewerModel.refreshUIMode()
        }
    }

    Connections {
        target: viewerModel
        function onViewerComponentTypeChanged() {
            // For Audio Units, reload the view instead of switching components
            if (viewerModel.viewerComponentType === ViewerComponentType.AudioUnit && prv.viewer) {
                prv.viewer.reload()
            } else {
                loadViewer()
            }
        }
    }

    function loadViewer() {
        switch (viewerModel.viewerComponentType) {
        case ViewerComponentType.AudioUnit:
            viewerLoader.sourceComponent = audioUnitViewerComponent
            break
        case ViewerComponentType.Lv2:
            viewerLoader.sourceComponent = lv2ViewerComponent
            break
        case ViewerComponentType.Vst:
            viewerLoader.sourceComponent = vstViewerComponent
            break
        case ViewerComponentType.Builtin:
            viewerLoader.sourceComponent = builtinViewerComponent
            break
        case ViewerComponentType.Generated:
            viewerLoader.sourceComponent = generatedViewerComponent
            break
        default:
            viewerLoader.sourceComponent = null
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
            instanceId: root.instanceId
            topPadding: topPanel.height
            minimumWidth: prv.minimumWidth
        }
    }

    Component {
        id: lv2ViewerComponent
        Lv2Viewer {
            instanceId: root.instanceId
            effectState: root.effectState // TODO: check if this is really needed !?
            title: root.title
        }
    }

    Component {
        id: vstViewerComponent
        VstViewer {
            instanceId: root.instanceId
            topPadding: topPanel.height
            minimumWidth: prv.minimumWidth
        }
    }

    Component {
        id: builtinViewerComponent
        BuiltinEffectViewer {
            instanceId: root.instanceId
            dialogView: root
            usedDestructively: false
        }
    }

    Component {
        id: generatedViewerComponent
        GeneratedEffectViewer {
            instanceId: root.instanceId
        }
    }

    Column {
        anchors.fill: parent

        WindowContainer {
            visible: prv.showTopPanel

            window: Window {
                id: topPanel

                width: root.contentWidth
                height: presetsBar.height + prv.separatorHeight + prv.panelMargins * 2

                color: ui.theme.backgroundPrimaryColor

                Row {
                    id: headerBar
                    spacing: 4
                    anchors.fill: parent
                    anchors.margins: prv.panelMargins

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

                        anchors.left: bypassBtn.right
                        anchors.leftMargin: headerBar.spacing
                        anchors.right: parent.right

                        destructiveMode: false
                        realtimeEffectState: root.effectState
                        navigationPanel: root.navigationPanel
                        navigationOrder: 1

                        parentWindow: root.window
                        instanceId: root.instanceId
                    }
                }

                SeparatorLine {
                    id: separator

                    anchors.top: headerBar.bottom
                    anchors.left: parent.left
                    anchors.right: parent.right

                    visible: (viewerModel.effectFamily == EffectFamily.Builtin || viewerModel.viewerComponentType == ViewerComponentType.Generated)
                }
            }
        }

        Item {
            id: spacer

            visible: !prv.showTopPanel

            width: parent.width
            height: prv.viewMargins
        }

        Loader {
            id: viewerLoader

            anchors.left: parent.left
            anchors.margins: prv.viewMargins
        }
    }
}
