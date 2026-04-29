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

    property int effectFamily: EffectFamily.Unknown

    title: viewerModel.title
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
        property bool showTopPanel: viewerModel.effectFamily !== EffectFamily.Builtin || (viewer && viewer.usesPresets)
        property bool showBottomPanel: true

        property bool isApplyAllowed: viewerModel.effectFamily != EffectFamily.Builtin || (viewer && viewer.isApplyAllowed)
        property bool isPreviewAllowed: !viewer || viewer.isPreviewAllowed !== false
        property bool shouldRollbackOnClose: true

        function closeWindow(accept) {
            if (prv.viewer) {
                prv.viewer.stopPreview()
            }
            // Call later because the preview calls `QCoreApplication::processEvents()`,
            // and we must make sure it doesn't do this after we've closed the dialog, or we'll be getting that Qt exception
            // "Object %p destroyed while one of its QML signal handlers is in progress."
            Qt.callLater(() => {
                prv.shouldRollbackOnClose = !accept
                root.activateParentOnClose = false
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

            if (prv.shouldRollbackOnClose) {
                viewerModel.rollbackSettings()
                presetsBar.presetsBarModel.restoreInitialPresetState()
            }
        }
    }

    Component.onCompleted: {
        viewerModel.load()
        loadViewer()
    }

    onWindowChanged: {
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

    DestructiveEffectViewerDialogModel {
        id: viewerModel
        instanceId: root.instanceId
    }

    Component {
        id: audioUnitViewerComponent
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
        id: lv2ViewerComponent
        Lv2Viewer {
            instanceId: root.instanceId
            title: root.title
        }
    }

    Component {
        id: vstViewerComponent
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
        id: builtinViewerComponent
        BuiltinEffectViewer {
            instanceId: root.instanceId
            dialogView: root
            usedDestructively: true
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

                    EffectPresetsBar {
                        id: presetsBar

                        width: parent.width

                        destructiveMode: true
                        navigationPanel: root.navigationPanel
                        navigationOrder: 0

                        enabled: !(prv.viewer && prv.viewer.isPreviewing)
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
                        navigationPanel.section: root.navigationSection
                        navigationPanel.order: (prv.showTopPanel ? 1 : 0) + (viewerModel.effectFamily == EffectFamily.Builtin ? (prv.viewer ? prv.viewer.numNavigationPanels : 2) : 0)

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

                            visible: prv.isPreviewAllowed

                            navigation.panel: bbox.navigationPanel
                            navigation.order: 0

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
                            navigation.panel: bbox.navigationPanel
                            navigation.order: previewBtn.navigation.order + 1

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
                            navigation.panel: bbox.navigationPanel
                            navigation.order: cancelBtn.navigation.order + 1

                            text: qsTrc("global", "Apply")
                            buttonRole: ButtonBoxModel.AcceptRole
                            buttonId: ButtonBoxModel.Apply
                            accentButton: true
                            enabled: prv.isApplyAllowed

                            onClicked: {
                                presetsBar.presetsBarModel.commitSelectedPreset()
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
}
