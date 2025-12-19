/*
* Audacity: A Digital Audio Editor
*/
import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

Item {
    id: root

    property alias itemsModel: listView.model
    property var selectedItems: []

    property int itemsViewWidth: width * 0.75

    property NavigationSection navigationSection: null
    property alias navigationPanelStartOrder: navPanel.order
    property alias navigationPanelEndOrder: buttonsNavPanel.order

    signal setSelectedRequested(var itemCode, bool selected)
    signal selectAllRequested()
    signal deselectAllRequested()

    RoundedRectangle {
        id: content

        anchors.left: parent.left
        anchors.top: parent.top
        anchors.bottom: parent.bottom

        width: itemsViewWidth

        color: ui.theme.backgroundSecondaryColor

        border.color: ui.theme.strokeColor
        border.width: 1

        radius: 4

        NavigationPanel {
            id: navPanel
            name: "SelectionItemsPanel"
            section: root.navigationSection
            enabled: root.visible && root.enabled
            direction: NavigationPanel.Vertical
        }

        StyledListView {
            id: listView

            anchors.fill: parent
            anchors.margins: 16

            delegate: ListItemBlank {
                mouseArea.hoverEnabled: false
                hoverHitColor: "transparent"

                function requestActiveFocus() {
                    checkBox.navigation.requestActive()
                }

                CheckBox {
                    id: checkBox
                    anchors.margins: 4
                    anchors.verticalCenter: parent.verticalCenter
                    anchors.left: parent.left
                    anchors.right: parent.right

                    text: modelData.title
                    font: modelData.isBold ? ui.theme.bodyBoldFont : ui.theme.bodyFont

                    checked: root.selectedItems.includes(modelData.itemId)

                    navigation.name: "ItemCheckBox " + text
                    navigation.panel: navPanel
                    navigation.row: model.index

                    onClicked: {
                        root.setSelectedRequested(modelData.itemId, !checked)
                    }
                }

                onFocusChanged: {
                    if (activeFocus) {
                        listView.positionViewAtIndex(index, ListView.Contain)
                    }
                }
            }
        }
    }

    ColumnLayout {
        id: buttons

        anchors.left: content.right
        anchors.leftMargin: 8
        anchors.right: parent.right
        anchors.top: parent.top

        spacing: 8

        NavigationPanel {
            id: buttonsNavPanel
            name: "ButtonsPanel"
            section: root.navigationSection
            direction: NavigationPanel.Horizontal
        }

        FlatButton {
            Layout.fillWidth: true

            text: qsTrc("global", "Select all")

            navigation.name: "SelectAllButton"
            navigation.panel: buttonsNavPanel
            navigation.column: 1

            onClicked: {
                root.selectAllRequested()
            }
        }

        FlatButton {
            Layout.fillWidth: true

            text: qsTrc("global", "Clear")

            navigation.name: "ClearButton"
            navigation.panel: buttonsNavPanel
            navigation.order: 2

            onClicked: {
                root.deselectAllRequested()
            }
        }
    }
}
