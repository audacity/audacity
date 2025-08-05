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

    property alias instanceId: viewerModel.instanceId
    property int effectFamily: EffectFamily.Unknown

    QtObject {
        id: prv
        property alias viewer: viewerLoader.item
        property bool isApplyAllowed: effectFamily != EffectFamily.Builtin || (viewer && viewer.isApplyAllowed)
        property int separatorHeight: effectFamily == EffectFamily.Builtin ? separator.height + root.margins : 0
        property bool showPresets: effectFamily != EffectFamily.Builtin || viewer.usesPresets
    }

    title: viewerModel.title

    minimumWidth: effectFamily === EffectFamily.LV2 ? 500 : 250
    implicitWidth: viewerLoader.width
    implicitHeight: {
        let height = viewerLoader.height + btnBarLoader.height + margins
        if (prv.showPresets) {
            height += presetsBar.height + margins + prv.separatorHeight
        }
        return height
    }

    margins: effectFamily == EffectFamily.Builtin ? 16 : 4

    onWindowChanged: {
        // Wait until the window is set: VstView needs it for intialization
        switch (effectFamily) {
            case EffectFamily.Builtin:
                viewerLoader.sourceComponent = builtinViewerComp
                break
            case EffectFamily.AudioUnit:
                viewerLoader.sourceComponent = audioUnitViewerComp
                break
            case EffectFamily.LV2:
                viewerLoader.sourceComponent = lv2ViewerComp
                break
            case EffectFamily.VST3:
                viewerLoader.sourceComponent = vstViewerComp
                break
            default:
                viewerLoader.sourceComponent = null
        }
    }

    Component.onCompleted: {
        // Delay loading of ButtonBox because it needs to know the final width before executing its layout
        // (which it only does once)
        btnBarLoader.sourceComponent = bboxComponent
    }

    EffectViewerDialogModel {
        id: viewerModel
    }

    Column {
        id: column
        anchors.fill: parent

        spacing: root.margins

        EffectPresetsBar {
            id: presetsBar
            visible: prv.showPresets
            navigationPanel: root.navigationPanel
            navigationOrder: 0
            instanceId: root.instanceId
            anchors.left: parent.left
            anchors.right: parent.right
        }

        SeparatorLine {
            id: separator
            visible: effectFamily == EffectFamily.Builtin && prv.showPresets
        }

        Component {
            id: builtinViewerComp
            BuiltinEffectViewer {
                instanceId: root.instanceId
                dialogView: root
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
                instanceId: root.instanceId
                height: implicitHeight
                topPadding: root.margins * 2 + presetsBar.height + prv.separatorHeight
                bottomPadding: btnBarLoader.implicitHeight + 2 * root.margins
                sidePadding: root.margins
                minimumWidth: root.minimumWidth
            }
        }

        Component {
            id: vstViewerComp
            VstViewer {
                instanceId: root.instanceId
                height: implicitHeight
                topPadding: root.margins * 2 + presetsBar.height + prv.separatorHeight
                bottomPadding: btnBarLoader.implicitHeight + 2 * root.margins
                sidePadding: root.margins
                minimumWidth: root.minimumWidth
            }
        }

        Loader {
            id: viewerLoader
        }

        Component {
            id: bboxComponent
            ButtonBox {
                id: bbox

                width: root.contentWidth
                spacing: root.margins

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
                    text: qsTrc("effects", "Preview")
                    buttonRole: ButtonBoxModel.CustomRole
                    buttonId: ButtonBoxModel.CustomButton + 2
                    isLeftSide: true
                    minWidth: 80
                    onClicked: prv.viewer.preview()
                    enabled: prv.isApplyAllowed
                    height: presetsBar.height
                }

                FlatButton {
                    id: cancelBtn
                    text: qsTrc("global", "Cancel")
                    buttonRole: ButtonBoxModel.RejectRole
                    buttonId: ButtonBoxModel.Cancel
                    minWidth: 80
                    onClicked: root.reject()
                    height: presetsBar.height
                }

                FlatButton {
                    id: okBtn
                    text: qsTrc("global", "Apply")
                    buttonRole: ButtonBoxModel.AcceptRole
                    buttonId: ButtonBoxModel.Apply
                    minWidth: 80
                    accentButton: true
                    onClicked: root.accept()
                    enabled: prv.isApplyAllowed
                    height: presetsBar.height
                }
            }
        }

        Loader {
            id: btnBarLoader
        }
    }
}
