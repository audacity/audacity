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

    property int effectFamily: EffectFamily.Unknown

    QtObject {
        id: prv
        property alias viewer: viewerLoader.item
        property bool isApplyAllowed: viewerModel.effectFamily != EffectFamily.Builtin || (viewer && viewer.isApplyAllowed)
        property bool showPresets: viewerModel.effectFamily != EffectFamily.Builtin || (viewer && viewer.usesPresets)

        property int minimumWidth: viewerModel.effectFamily === EffectFamily.LV2 ? 500 : 250
        property int panelMargins: viewerModel.effectFamily == EffectFamily.Builtin ? 16 : 4
        property int viewMargins: viewerModel.effectFamily == EffectFamily.Builtin ? 16 : 0
        property int separatorHeight: viewerModel.effectFamily == EffectFamily.Builtin ? separator.height + prv.panelMargins : 0

        function closeWindow(accept) {
            if (prv.viewer) {
                prv.viewer.stopPreview()
            }
            // Call later because the preview calls `QCoreApplication::processEvents()`,
            // and we must make sure it doesn't do this after we've closed the dialog, or we'll be getting that Qt exception
            // "Object %p destroyed while one of its QML signal handlers is in progress."
            Qt.callLater(() => {
                accept ? root.accept() : root.reject()
            })
        }
    }

    Connections {
        target: root.window
        function onClosing(event) {
            // Stop preview before closing, for the same reason as in closeWindow()
            if (prv.viewer) {
                prv.viewer.stopPreview()
            }
        }
    }

    title: viewerModel.title

    contentWidth: Math.max(viewerLoader.width + prv.viewMargins * 2, prv.minimumWidth)
    contentHeight: {
        let height = viewerLoader.height + bottomPanel.height
        height += prv.showPresets ? topPanel.height : prv.viewMargins
        return height
    }

    Component.onCompleted: {
        viewerModel.load()
    }

    onWindowChanged: {
        loadViewer()
    }

    Connections {
        target: viewerModel
        function onViewerComponentTypeChanged() {
            // For Audio Units, reload the view instead of switching components
            if (viewerModel.viewerComponentType === ViewerComponentType.AudioUnit && viewerLoader.item) {
                viewerLoader.item.reload()
            } else {
                loadViewer()
            }
        }
    }

    // Listen to UI mode changes from the presets bar menu
    Connections {
        target: presetsBar.manageMenuModel
        function onUseVendorUIChanged() {
            viewerModel.refreshUIMode()
        }
    }

    function loadViewer() {
        switch (viewerModel.viewerComponentType) {
        case ViewerComponentType.AudioUnit:
            viewerLoader.sourceComponent = audioUnitViewerComp
            break
        case ViewerComponentType.Lv2:
            viewerLoader.sourceComponent = lv2ViewerComp
            break
        case ViewerComponentType.Vst:
            viewerLoader.sourceComponent = vstViewerComp
            break
        case ViewerComponentType.Builtin:
            viewerLoader.sourceComponent = builtinViewerComp
            break
        case ViewerComponentType.Generated:
            viewerLoader.sourceComponent = generatedViewerComp
            break
        default:
            viewerLoader.sourceComponent = null
        }
    }

    DestructiveEffectViewerDialogModel {
        id: viewerModel
    }

    Column {
        id: column
        anchors.fill: parent

        WindowContainer {
            visible: prv.showPresets

            window: Window {
                id: topPanel

                width: root.contentWidth
                height: presetsBar.height + prv.separatorHeight + prv.panelMargins * 2

                color: ui.theme.backgroundPrimaryColor

                Column {
                    id: presetsBarContainer

                    anchors.fill: parent
                    anchors.margins: prv.panelMargins

                    spacing: prv.panelMargins

                    EffectPresetsBar {
                        id: presetsBar

                        anchors.left: parent.left
                        anchors.right: parent.right

                        navigationPanel: root.navigationPanel
                        navigationOrder: 0

                        enabled: !(prv.viewer && prv.viewer.isPreviewing)
                        parentWindow: root.window
                        instanceId: root.instanceId
                    }
                }

                SeparatorLine {
                    id: separator

                    anchors.top: presetsBarContainer.bottom
                    anchors.left: parent.left
                    anchors.right: parent.right

                    visible: viewerModel.effectFamily == EffectFamily.Builtin
                }
            }
        }

        Item {
            id: spacer

            visible: !prv.showPresets

            width: parent.width
            height: prv.viewMargins
        }

        Loader {
            id: viewerLoader

            anchors.left: parent.left
            anchors.margins: prv.viewMargins
        }

        WindowContainer {
            window: Window {
                id: bottomPanel

                width: root.contentWidth
                height: prv.panelMargins * 2 + bbox.height

                color: ui.theme.backgroundPrimaryColor

                Item {

                    anchors.fill: parent
                    anchors.margins: prv.panelMargins

                    ButtonBox {
                        id: bbox

                        width: root.contentWidth - prv.panelMargins * 2
                        spacing: prv.panelMargins

                        //! TODO Move function to ButtonBox (Muse framework)
                        function buttonById(id) {
                            for (var i = 0; i < bbox.count; i++) {
                                var btn = bbox.itemAt(i)
                                if (btn.buttonId === id) {
                                    return btn
                                }
                            }

                            return null
                        }

                        FlatButton {
                            id: previewBtn

                            height: presetsBar.height
                            minWidth: 80
                            isLeftSide: true

                            text: (prv.viewer && prv.viewer.isPreviewing) ?
                            //: Shown on a button that stops effect preview
                            qsTrc("effects", "Stop preview") :
                            //: Shown on a button that starts effect preview
                            qsTrc("effects", "Preview")

                            buttonRole: ButtonBoxModel.CustomRole
                            buttonId: ButtonBoxModel.CustomButton + 2
                            enabled: prv.isApplyAllowed

                            onClicked: {
                                if (!prv.viewer) {
                                    return
                                }
                                if (prv.viewer.isPreviewing) {
                                    prv.viewer.stopPreview()
                                } else {
                                    prv.viewer.startPreview()
                                }
                            }
                        }

                        FlatButton {
                            id: cancelBtn

                            height: presetsBar.height
                            minWidth: 80

                            text: qsTrc("global", "Cancel")
                            buttonRole: ButtonBoxModel.RejectRole
                            buttonId: ButtonBoxModel.Cancel

                            onClicked: {
                                prv.closeWindow(false)
                            }
                        }

                        FlatButton {
                            id: okBtn

                            height: presetsBar.height
                            minWidth: 80

                            text: qsTrc("global", "Apply")
                            buttonRole: ButtonBoxModel.AcceptRole
                            buttonId: ButtonBoxModel.Apply
                            accentButton: true
                            enabled: prv.isApplyAllowed

                            onClicked: {
                                prv.closeWindow(true)
                            }
                        }
                    }
                }
            }
        }
    }

    EffectControlsDisablingOverlay {
        x: viewerLoader.x
        y: viewerLoader.y
        width: viewerLoader.width
        height: viewerLoader.height

        visible: prv.viewer && prv.viewer.isPreviewing
        effectFamily: root.effectFamily
    }

    Component {
        id: builtinViewerComp
        BuiltinEffectViewer {
            instanceId: root.instanceId
            dialogView: root
            usedDestructively: true
        }
    }

    Component {
        id: lv2ViewerComp
        Lv2Viewer {
            instanceId: root.instanceId
            title: root.title
        }
    }

    Component {
        id: audioUnitViewerComp
        AudioUnitViewer {
            height: implicitHeight

            instanceId: root.instanceId
            topPadding: topPanel.height
            bottomPadding: bbox.implicitHeight + prv.panelMargins * 2
            sidePadding: prv.viewMargins
            minimumWidth: prv.minimumWidth
        }
    }

    Component {
        id: vstViewerComp
        VstViewer {
            height: implicitHeight

            instanceId: root.instanceId
            topPadding: topPanel.height
            bottomPadding: bbox.implicitHeight + prv.panelMargins * 2
            sidePadding: prv.viewMargins
            minimumWidth: prv.minimumWidth
        }
    }

    Component {
        id: generatedViewerComp
        GeneratedEffectViewer {
            instanceId: root.instanceId
        }
    }
}
