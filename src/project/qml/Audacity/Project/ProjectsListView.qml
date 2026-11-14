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

    property AbstractItemModel model
    property list<ColumnItem> columns
    property alias showNewProjectItem: newProjectItem.visible
    property string searchText

    property color backgroundColor: ui.theme.backgroundSecondaryColor
    property real sideMargin: 46
    property real _columnsContentX: 0

    readonly property real _remainingColumnsMinWidth: {
        var total = 0
        for (var i = 1; i < columns.length; i++) {
            total += columns[i].width
        }
        if (columns.length > 2) {
            total += (columns.length - 2) * 24
        }
        return total
    }

    property alias view: view

    property alias navigation: navPanel

    signal createNewProjectRequested
    signal openProjectRequested(var projectPath, var displayName)
    signal openCloudProjectRequested(var projectId)

    function scrollColumnIntoView(columnX, columnWidth) {
        var viewport = headerFlickable.width
        var cx = headerFlickable.contentX

        if (columnX < cx) {
            headerFlickable.contentX = columnX
        } else if (columnX + columnWidth > cx + viewport) {
            headerFlickable.contentX = columnX + columnWidth - viewport
        }
    }

    component ColumnItem: QtObject {
        property string header
        property real width: 0
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
        name: "ProjectsListView"
        direction: NavigationPanel.Both
        accessible.name: qsTrc("project", "Projects list")
    }

    Component {
        id: headerLabelComp

        StyledTextLabel {
            text: headerText

            font: Qt.font(Object.assign({}, ui.theme.bodyBoldFont, {
                capitalization: Font.AllUppercase
            }))
            horizontalAlignment: Text.AlignLeft
        }
    }

    ColumnLayout {
        anchors.fill: parent
        anchors.leftMargin: sideMargin
        anchors.rightMargin: sideMargin

        spacing: 12

        ListItemBlank {
            id: newProjectItem

            Layout.fillWidth: true
            Layout.alignment: Qt.AlignTop | Qt.AlignHCenter
            implicitHeight: view.rowHeight

            visible: false

            navigation.panel: navPanel
            navigation.row: 0
            navigation.column: 0

            onClicked: {
                root.createNewProjectRequested()
            }

            RowLayout {
                anchors.fill: parent
                anchors.leftMargin: view.itemInset
                anchors.rightMargin: view.itemInset

                spacing: view.columnSpacing

                Rectangle {
                    Layout.preferredWidth: 90
                    Layout.preferredHeight: 48

                    color: ui.theme.extra["white_color"]

                    StyledIconLabel {
                        anchors.centerIn: parent

                        iconCode: IconCode.PLUS

                        font.pixelSize: 16
                        color: ui.theme.extra["black_color"]
                    }
                }

                StyledTextLabel {
                    id: projectName

                    Layout.preferredWidth: 100

                    text: qsTrc("project", "New project")
                    font: ui.theme.largeBodyFont
                    horizontalAlignment: Text.AlignLeft
                }

                Item {
                    Layout.fillWidth: true
                }
            }
        }

        Item {
            id: listViewContainer

            Layout.fillWidth: true
            Layout.fillHeight: true

            visible: view.count > 0 || view.header || view.footer

            ColumnLayout {
                id: listViewColumn

                anchors.fill: parent
                spacing: 0

                // Column headers
                RowLayout {
                    id: headerLayout

                    Layout.fillWidth: true
                    Layout.preferredHeight: 44
                    Layout.leftMargin: view.itemInset
                    Layout.rightMargin: view.itemInset

                    spacing: view.columnSpacing

                    Loader {
                        readonly property var columnData: root.columns.length > 0 ? root.columns[0] : null

                        active: columnData !== null

                        Layout.preferredHeight: parent.height
                        Layout.preferredWidth: columnData ? columnData.width : 0
                        Layout.minimumWidth: columnData ? columnData.width : 0
                        Layout.fillWidth: true

                        sourceComponent: columnData ? headerLabelComp : null

                        readonly property string headerText: columnData ? columnData.header : ""
                    }

                    Item {
                        Layout.preferredHeight: parent.height
                        Layout.preferredWidth: root._remainingColumnsMinWidth
                        Layout.maximumWidth: root._remainingColumnsMinWidth
                        Layout.minimumWidth: 0
                        Layout.fillWidth: true

                        Flickable {
                            id: headerFlickable

                            anchors.fill: parent
                            clip: true

                            flickableDirection: Flickable.HorizontalFlick
                            contentWidth: root._remainingColumnsMinWidth
                            contentHeight: height
                            boundsBehavior: Flickable.StopAtBounds

                            onContentXChanged: {
                                root._columnsContentX = contentX
                            }

                            RowLayout {
                                id: headerColumnsRow
                                width: headerFlickable.contentWidth
                                height: headerFlickable.height
                                spacing: view.columnSpacing

                                Repeater {
                                    id: headerColumnsRepeater
                                    model: Math.max(root.columns.length - 1, 0)

                                    delegate: Loader {
                                        readonly property var columnData: root.columns[model.index + 1]

                                        Layout.preferredWidth: columnData.width
                                        Layout.minimumWidth: columnData.width

                                        sourceComponent: headerLabelComp

                                        readonly property string headerText: columnData.header
                                    }
                                }
                            }

                            ScrollBar.horizontal: StyledScrollBar {
                                parent: listViewContainer

                                anchors.left: parent.left
                                anchors.right: parent.right
                                anchors.bottom: parent.bottom

                                policy: ScrollBar.AlwaysOn

                                visible: headerFlickable.contentWidth > headerFlickable.width
                                z: 2
                            }
                        }
                    }
                }

                SeparatorLine {}

                StyledListView {
                    id: view

                    Layout.fillWidth: true
                    Layout.fillHeight: true

                    bottomMargin: bottomGradient.height

                    readonly property real itemInset: 12
                    readonly property real rowHeight: 64
                    readonly property real columnSpacing: 24

                    readonly property int cellHeight: rowHeight + spacing

                    ScrollBar.vertical: StyledScrollBar {
                        parent: root

                        anchors.top: parent.top
                        anchors.bottom: parent.bottom
                        anchors.right: parent.right

                        policy: ScrollBar.AlwaysOn

                        visible: view.contentHeight > view.height
                        z: 2
                    }

                    model: searchFilterModel

                    delegate: ProjectListItem {
                        required property int index

                        columns: root.columns
                        columnsContentX: root._columnsContentX
                        columnsMinWidth: root._remainingColumnsMinWidth
                        showBottomBorder: index < view.count - 1

                        itemInset: view.itemInset
                        implicitHeight: view.rowHeight
                        columnSpacing: view.columnSpacing

                        navigation.panel: navPanel
                        navigation.row: index + 1
                        navigation.column: 0

                        onColumnScrollRequested: function (columnX, columnWidth) {
                            root.scrollColumnIntoView(columnX, columnWidth)
                        }

                        onClicked: {
                            if (item.isCloud) {
                                root.openCloudProjectRequested(item.itemId)
                            } else {
                                root.openProjectRequested(item.path, item.name)
                            }
                        }
                    }
                }
            }

            Rectangle {
                id: bottomGradient
                anchors.left: parent.left
                anchors.right: parent.right
                anchors.bottom: parent.bottom

                visible: listViewColumn.visible

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
