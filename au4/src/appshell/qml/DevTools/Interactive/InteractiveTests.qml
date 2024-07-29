/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.7
import Muse.Ui 1.0
import Muse.UiComponents 1.0

Rectangle {

    color: "#71C2EF"

    NavigationSection {
        id: navSec
        name: "InteractiveTests"
        order: 10
    }

    NavigationPanel {
        id: navPanel
        name: "InteractiveTests"
        section: navSec
        order: 1
        direction: NavigationPanel.Vertical

        accessible.name: "InteractiveTests"
    }

    InteractiveTestsModel {
        id: testModel
    }

    Component.onCompleted: {
        testModel.init()
    }

    StyledTextLabel {
        id: header
        width: parent.width
        height: 40
        text: testModel.currentUri
    }

    Grid {
        anchors.top: header.bottom
        anchors.bottom: parent.bottom
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.margins: 16
        spacing: 16
        columns: 2


        FlatButton {
            width: 200
            navigation.panel: navPanel
            navigation.row: 0
            text: "[cpp] Sample dialog"
            onClicked: testModel.openSampleDialog()
        }

        FlatButton {
            width: 200
            navigation.panel: navPanel
            navigation.row: 1
            text: "[qml] Sample dialog"
            onClicked: {
                console.log("qml: before open")
                api.launcher.open("musescore://devtools/interactive/sample?color=#0F9D58&isApplyColor=true")
                console.log("qml: after open")
            }
        }

        FlatButton {
            width: 200
            navigation.panel: navPanel
            navigation.row: 2
            text: "[cpp] Sample dialog async"
            onClicked: testModel.openSampleDialogAsync()
        }

        FlatButton {
            width: 200
            navigation.panel: navPanel
            navigation.row: 3
            text: "[qml] Sample dialog sync"
            onClicked: {
                console.log("qml: before open")
                api.launcher.open("musescore://devtools/interactive/sample?sync=true&color=#EF8605")
                console.log("qml: after open")
            }
        }

        FlatButton {
            width: 200
            navigation.panel: navPanel
            navigation.row: 4
            text: "[qml] Sample dialog modal"
            onClicked: {
                console.log("qml: before open")
                api.launcher.open("musescore://devtools/interactive/sample?modal=true&color=#D13F31")
                console.log("qml: after open")
            }
        }

        FlatButton {
            width: 200
            navigation.panel: navPanel
            navigation.row: 5
            text: "[cpp] Sample dialog close"
            onClicked: testModel.closeSampleDialog()
        }

        FlatButton {
            width: 200
            navigation.panel: navPanel
            navigation.row: 6
            text: "Open musescore.com"
            onClicked: {
                api.launcher.openUrl("https://musescore.com/")
            }
        }

        FlatButton {
            width: 200
            navigation.panel: navPanel
            navigation.row: 7
            text: "Question"
            onClicked: testModel.question()
        }

        FlatButton {
            width: 200
            navigation.panel: navPanel
            navigation.row: 8
            text: "Custom question"
            onClicked: testModel.customQuestion()
        }

        FlatButton {
            width: 200
            navigation.panel: navPanel
            navigation.row: 9
            text: "Information"
            onClicked: testModel.information()
        }

        FlatButton {
            width: 200
            navigation.panel: navPanel
            navigation.row: 10
            text: "Warning"
            onClicked: testModel.warning()
        }

        FlatButton {
            width: 200
            navigation.panel: navPanel
            navigation.row: 11
            text: "Critical"
            onClicked: testModel.critical()
        }

        FlatButton {
            width: 200
            navigation.panel: navPanel
            navigation.row: 10
            text: "Critical with detailed text"
            onClicked: testModel.criticalWithDetailedText()
        }

        FlatButton {
            width: 200
            navigation.panel: navPanel
            navigation.row: 12
            text: "Require"
            onClicked: testModel.require()
        }

        FlatButton {
            width: 200
            navigation.panel: navPanel
            navigation.row: 13
            text: "Widget dialog"
            onClicked: testModel.openWidgetDialog()
        }

        FlatButton {
            width: 200
            navigation.panel: navPanel
            navigation.row: 14
            text: "Widget dialog async"
            onClicked: testModel.openWidgetDialogAsync()
        }

        FlatButton {
            width: 200
            navigation.panel: navPanel
            navigation.row: 15
            text: "Widget dialog close"
            onClicked: testModel.closeWidgetDialog()
        }
    }
}
