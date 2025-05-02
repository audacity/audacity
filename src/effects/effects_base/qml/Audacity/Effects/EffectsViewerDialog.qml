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

    property var instanceId
    property bool isVst: false

    property alias viewer: viewerLoader.item
    property bool isApplyAllowed: isVst || (viewer && viewer.isApplyAllowed)

    title: viewer ? viewer.title : ""

    contentWidth: Math.max(viewerLoader.width, 300)
    contentHeight: presetsBar.height + separator.height + viewerLoader.height + btnBarLoader.height + margins * 3

    margins: 16

    onWindowChanged: {
        // Wait until the window is set: VstView needs it for intialization
        viewerLoader.sourceComponent = isVst ? vstViewerComp : builtinViewerComp
    }

    Component.onCompleted: {
        // Delay loading of ButtonBox because it needs to know the final width before executing its layout
        // (which it only does once)
        btnBarLoader.sourceComponent = bboxComponent
    }

    Column {
        id: column
        anchors.fill: parent

        spacing: 16

        EffectPresetsBar {
            id: presetsBar
            navigationPanel: root.navigationPanel
            navigationOrder: 0
            instanceId: root.instanceId
            anchors.left: parent.left
            anchors.right: parent.right
        }

        SeparatorLine {
            id: separator
        }

        Component {
            id: builtinViewerComp
            EffectsViewer {
                instanceId: root.instanceId
            }
        }

        Component {
            id: vstViewerComp
            VstViewer {
                instanceId: root.instanceId
                height: implicitHeight
                x: root.margins
                y: root.margins * 3 + presetsBar.height + separator.height
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
                    onClicked: viewer.preview()
                    enabled: root.isApplyAllowed
                }

                FlatButton {
                    id: cancelBtn
                    text: qsTrc("global", "Cancel")
                    buttonRole: ButtonBoxModel.RejectRole
                    buttonId: ButtonBoxModel.Cancel
                    minWidth: 80
                    onClicked: root.reject()
                }

                FlatButton {
                    id: okBtn
                    text: qsTrc("global", "Apply")
                    buttonRole: ButtonBoxModel.AcceptRole
                    buttonId: ButtonBoxModel.Apply
                    minWidth: 80
                    accentButton: true
                    onClicked: root.accept()
                    enabled: root.isApplyAllowed
                }
            }
        }

        Loader {
            id: btnBarLoader
        }
    }
}
