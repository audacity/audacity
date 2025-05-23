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

    QtObject {
        id: prv
        property alias viewer: viewerLoader.item
        property bool isApplyAllowed: isVst || (viewer && viewer.isApplyAllowed)
        property int separatorHeight: isVst ? 0 : separator.height + root.margins
    }

    title: prv.viewer ? prv.viewer.title : ""

    minimumWidth: 250
    implicitWidth: viewerLoader.width
    implicitHeight: presetsBar.height + viewerLoader.height + btnBarLoader.height + margins * 2 + prv.separatorHeight

    margins: isVst ? 4 : 16

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

        spacing: root.margins

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
            visible: !isVst
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
