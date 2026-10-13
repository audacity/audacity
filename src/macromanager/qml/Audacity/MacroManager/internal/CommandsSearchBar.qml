/*
* Audacity: A Digital Audio Editor
*/
pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

Item {
    id: root

    property string searchText: ""

    property NavigationPanel navigationPanel: NavigationPanel {
        name: "CommandsSearchBar"
        direction: NavigationPanel.Horizontal
        enabled: root.enabled && root.visible
        accessible.name: qsTrc("macromanager", "Search commands")
    }

    signal searchTextChangeRequested(string text)
    signal searchAccepted

    implicitHeight: prv.height

    QtObject {
        id: prv

        readonly property int height: 44
        readonly property int sideMargin: 8
        readonly property int fieldHeight: 28
    }

    RowLayout {
        anchors.fill: parent
        anchors.leftMargin: prv.sideMargin
        anchors.rightMargin: prv.sideMargin

        TextInputField {
            id: searchField

            Layout.fillWidth: true
            Layout.preferredHeight: prv.fieldHeight

            currentText: root.searchText
            hint: qsTrc("macromanager", "Search")
            hintIcon: IconCode.SEARCH
            clearTextButtonVisible: true

            navigation.panel: root.navigationPanel
            navigation.order: 0
            navigation.name: "CommandsSearchField"
            navigation.accessible.name: hint

            onTextChanged: function (newTextValue) {
                root.searchTextChangeRequested(newTextValue)
            }

            onAccepted: {
                root.searchAccepted()
            }
        }
    }
}
