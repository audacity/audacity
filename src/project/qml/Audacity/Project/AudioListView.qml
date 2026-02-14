/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Layouts 1.15

import Muse.Ui 1.0
import Muse.UiComponents
import Audacity.Project 1.0

import "internal/ProjectsPage"

Item {
    id: root

    property AbstractProjectsModel model
    property list<ColumnItem> columns
    property string searchText

    property color backgroundColor: ui.theme.backgroundSecondaryColor
    property real sideMargin: 46

    property color itemNormalColor: "transparent"
    property color itemHoverHitColor: ui.theme.buttonColor
    property real itemSpacing: 0

    property alias view: view

    property alias navigation: navPanel

    component ColumnItem : QtObject {
        property var width: function (parentWidth) {
            return parentWidth / 5
        }

        property Component delegate
    }

    SortFilterProxyModel {
        id: searchFilterModel
        sourceModel: root.model

        alwaysExcludeIndices: root.model.nonProjectItemIndices

        filters: [
            FilterValue {
                roleName: "name"
                roleValue: root.searchText
                compareType: CompareType.Contains
            }
        ]
    }

    NavigationPanel {
        id: navPanel
        name: "AudioListView"
        direction: NavigationPanel.Both
        accessible.name: qsTrc("project", "Audio files list")
    }

    ColumnLayout {
        anchors.fill: parent
        anchors.leftMargin: sideMargin
        anchors.rightMargin: sideMargin

        spacing: 12

        Item {
            id: listViewContainer

            Layout.fillWidth: true
            Layout.fillHeight: true

            visible: view.count > 0 || view.header || view.footer

            StyledListView {
                id: view
                anchors.fill: parent

                spacing: root.itemSpacing
                bottomMargin: bottomGradient.height

                readonly property real itemInset: 12
                readonly property real rowHeight: 48
                readonly property real columnSpacing: 44

                ScrollBar.vertical: StyledScrollBar {
                    parent: root

                    anchors.top: parent.top
                    anchors.bottom: parent.bottom
                    anchors.right: parent.right

                    visible: view.contentHeight > view.height
                    z: 2
                }

                model: searchFilterModel

                delegate: AudioListItem {
                    required property int index

                    columns: root.columns

                    itemInset: view.itemInset
                    implicitHeight: view.rowHeight
                    columnSpacing: view.columnSpacing

                    normalColor: root.itemNormalColor
                    hoverHitColor: root.itemHoverHitColor

                    navigation.panel: navPanel
                    navigation.row: index + 1
                    navigation.column: 0

                    onClicked: {}
                }
            }

            Rectangle {
                id: bottomGradient
                anchors.left: parent.left
                anchors.right: parent.right
                anchors.bottom: parent.bottom

                visible: view.visible

                height: 8
                z: 1

                gradient: Gradient {
                    GradientStop {
                        position: 0.0
                        color: "transparent"
                    }

                    GradientStop {
                        position: 1.0
                        color: root.backgroundColor
                    }
                }
            }
        }

        Item {
            id: noResultsMessage

            Layout.fillWidth: true
            Layout.fillHeight: true

            visible: Boolean(root.searchText) && !listViewContainer.visible

            Message {
                anchors.top: parent.top
                anchors.topMargin: Math.max(parent.height / 3 - height / 2, 0)
                anchors.left: parent.left
                anchors.leftMargin: root.sideMargin
                anchors.right: parent.right
                anchors.rightMargin: root.sideMargin

                title: qsTrc("global", "No results found")
            }
        }
    }
}
