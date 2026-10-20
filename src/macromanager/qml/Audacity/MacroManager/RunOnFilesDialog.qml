/*
* Audacity: A Digital Audio Editor
*/
pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

import Audacity.MacroManager

import "internal"

StyledDialogView {
    id: root

    property string macroName: ""
    property var files: []

    title: qsTrc("macromanager", "Run on files")

    contentWidth: prv.maximumWidth
    contentHeight: prv.maximumHeight
    resizable: true
    margins: 0
    modal: false

    QtObject {
        id: prv

        readonly property int minimumWidth: 320
        readonly property int minimumHeight: 200
        readonly property int maximumWidth: 640
        readonly property int maximumHeight: 400
        readonly property int buttonsRowHeight: 44
        readonly property int sideMargin: 8
        readonly property int buttonsSpacing: 8
        readonly property int buttonHeight: 28
    }

    onIsOpenedChanged: {
        if (root.isOpened) {
            root.contentWidth = prv.minimumWidth
            root.contentHeight = prv.minimumHeight
            root.window.maximumWidth = prv.maximumWidth
            root.window.maximumHeight = prv.maximumHeight
        }
    }

    RunOnFilesModel {
        id: runOnFilesModel
    }

    Component.onCompleted: {
        runOnFilesModel.load(root.files)
    }

    onNavigationActivateRequested: {
        buttonsRow.navigationPanel.requestActive()
    }

    ColumnLayout {
        anchors.fill: parent

        spacing: 0

        RunOnFilesTable {
            Layout.fillWidth: true
            Layout.fillHeight: true

            model: runOnFilesModel
        }

        SeparatorLine {}

        RowLayout {
            id: buttonsRow

            Layout.fillWidth: true
            Layout.preferredHeight: prv.buttonsRowHeight
            Layout.leftMargin: prv.sideMargin
            Layout.rightMargin: prv.sideMargin

            spacing: prv.buttonsSpacing

            property NavigationPanel navigationPanel: NavigationPanel {
                name: "RunOnFilesButtons"
                direction: NavigationPanel.Horizontal
                section: root.navigationSection
                order: 1
            }

            Item {
                Layout.fillWidth: true
            }

            FlatButton {
                Layout.preferredHeight: prv.buttonHeight

                text: qsTrc("macromanager", "Stop")
                enabled: runOnFilesModel.isRunning

                navigation.panel: buttonsRow.navigationPanel
                navigation.column: 0

                onClicked: {
                    runOnFilesModel.abort()
                }
            }

            FlatButton {
                Layout.preferredHeight: prv.buttonHeight

                text: qsTrc("global", "Close")
                enabled: !runOnFilesModel.isRunning

                navigation.panel: buttonsRow.navigationPanel
                navigation.column: 1

                onClicked: {
                    root.hide()
                    runOnFilesModel.closeWindow()
                }
            }
        }
    }
}
