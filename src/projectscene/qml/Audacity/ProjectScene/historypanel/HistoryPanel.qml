/*
* Audacity: A Digital Audio Editor
*/
import QtQuick
import QtQuick.Layouts
import QtQuick.Controls

import Muse.Ui
import Muse.UiComponents

import Audacity.ProjectScene

Item {
    id: root

    property alias navigationSection: navPanel.section
    property alias navigationOrderStart: navPanel.order

    NavigationPanel {
        id: navPanel
        name: "HistoryPanel"
        direction: NavigationPanel.Vertical
        enabled: root.enabled && root.visible
    }

    StyledListView {
        id: listView
        anchors.fill: parent

        model: HistoryPanelModel {
            id: historyPanelModel
        }

        currentIndex: historyPanelModel.currentIndex
        scrollBarPolicy: ScrollBar.AlwaysOn

        delegate: ListItemBlank {
            id: listItem
            isSelected: ListView.isCurrentItem

            readonly property bool isRedoable: model.index > historyPanelModel.currentIndex

            navigation.panel: navPanel
            navigation.order: model.index
            navigation.accessible.name: model.text
            navigation.accessible.row: model.index

            onClicked: {
                historyPanelModel.undoRedoToIndex(model.index)
            }

            RowLayout {
                anchors.fill: parent
                anchors.leftMargin: 12
                anchors.rightMargin: 12

                spacing: 6

                Item {
                    implicitWidth: 16
                    implicitHeight: checkMark.implicitHeight

                    StyledIconLabel {
                        id: checkMark
                        anchors.centerIn: parent
                        iconCode: IconCode.TICK_RIGHT_ANGLE
                        visible: listItem.ListView.isCurrentItem
                    }
                }

                StyledTextLabel {
                    Layout.fillWidth: true
                    horizontalAlignment: Text.AlignLeft

                    text: model.text
                    font: {
                        if (listItem.ListView.isCurrentItem) {
                            return ui.theme.bodyBoldFont
                        }

                        if (listItem.isRedoable) {
                            return Qt.font(Object.assign({}, ui.theme.bodyFont, { italic: true }))
                        }

                        return ui.theme.bodyFont
                    }

                    opacity: listItem.isRedoable ? 0.7 : 1
                }
            }
        }
    }
}
