/*
* Audacity: A Digital Audio Editor
*/
pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

import Audacity.MacroManager

StyledDialogView {
    id: root

    property string macroName: ""
    property var files: []

    title: qsTrc("macromanager", "Run on files")

    contentWidth: prv.contentWidth
    contentHeight: prv.contentHeight
    margins: prv.margins
    modal: false
    closeOnEscape: !runOnFilesModel.isRunning

    QtObject {
        id: prv

        readonly property int contentWidth: 480
        readonly property int contentHeight: 360
        readonly property int margins: 16
        readonly property int rowHeight: 44
        readonly property int rowSpacing: 16
        readonly property int progressBarWidth: 160
        readonly property int buttonsTopMargin: 16
        readonly property int buttonWidth: 64
        readonly property int buttonHeight: 28
    }

    RunOnFilesModel {
        id: runOnFilesModel
    }

    Component.onCompleted: {
        runOnFilesModel.load(root.files)
    }

    onNavigationActivateRequested: {
        buttons.navigationPanel.requestActive()
    }

    ColumnLayout {
        anchors.fill: parent

        spacing: 0

        StyledTextLabel {
            Layout.fillWidth: true

            text: root.macroName
            font: ui.theme.bodyBoldFont
            horizontalAlignment: Qt.AlignLeft
        }

        StyledListView {
            Layout.fillWidth: true
            Layout.fillHeight: true
            Layout.topMargin: prv.rowSpacing

            model: runOnFilesModel

            delegate: RowLayout {
                id: row

                required property int index
                required property string fileName
                required property double progress

                width: ListView.view.width
                height: prv.rowHeight

                spacing: prv.rowSpacing

                StyledTextLabel {
                    Layout.fillWidth: true

                    text: row.fileName
                    horizontalAlignment: Qt.AlignLeft
                    elide: Text.ElideMiddle
                }

                ProgressBar {
                    Layout.preferredWidth: prv.progressBarWidth

                    from: 0.0
                    to: 1.0
                    value: row.progress
                    progressStatus: Math.round(row.progress * 100) + "%"
                }
            }
        }

        ButtonBox {
            id: buttons

            Layout.fillWidth: true
            Layout.topMargin: prv.buttonsTopMargin

            navigationPanel.section: root.navigationSection
            navigationPanel.order: 1

            FlatButton {
                Layout.preferredWidth: prv.buttonWidth
                Layout.preferredHeight: prv.buttonHeight

                text: runOnFilesModel.isRunning ? qsTrc("global", "Cancel") : qsTrc("global", "Close")
                buttonRole: ButtonBoxModel.RejectRole
                buttonId: ButtonBoxModel.Cancel

                onClicked: {
                    if (runOnFilesModel.isRunning) {
                        runOnFilesModel.abort()
                    } else {
                        root.hide()
                    }
                }
            }
        }
    }
}
