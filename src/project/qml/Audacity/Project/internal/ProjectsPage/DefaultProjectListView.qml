/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15
import QtQuick.Controls 2.15

import Muse.Ui 1.0
import Muse.UiComponents
import Audacity.Project 1.0

Item {
    id: root

    property var model
    property int viewType: ProjectsPageModel.Grid
    property string searchText

    property color backgroundColor: ui.theme.backgroundSecondaryColor
    property real sideMargin: 46

    property bool showNewProjectItem: false
    property bool cloudIndicatorAlwaysVisible: false
    property string placeholder: ":/resources/ProjectPlaceholder.svg"

    property NavigationSection navigationSection
    property int navigationOrder
    property string navigationName: "Projects"
    property string gridAccessibleName: ""
    property string listAccessibleName: ""

    readonly property var view: loader.item ? loader.item.view : null

    signal createNewProjectRequested
    signal openProjectRequested(var projectPath, var displayName)
    signal openCloudProjectRequested(var projectId, var projectPath, var displayName)

    Loader {
        id: loader
        anchors.fill: parent
        sourceComponent: root.viewType === ProjectsPageModel.List ? listComp : gridComp
    }

    Component {
        id: cloudIndicatorComp

        CloudProjectIndicatorButton {
            mouseArea.enabled: false

            isProgress: false
            isDownloadedAndUpToDate: true
        }
    }

    Component {
        id: gridComp

        ProjectsGridView {
            anchors.fill: parent

            model: root.model
            searchText: root.searchText

            backgroundColor: root.backgroundColor
            sideMargin: root.sideMargin
            placeholder: root.placeholder

            indicatorButton: cloudIndicatorComp

            navigation.section: root.navigationSection
            navigation.order: root.navigationOrder
            navigation.name: root.navigationName + "Grid"
            navigation.accessible.name: root.gridAccessibleName

            onCreateNewProjectRequested: root.createNewProjectRequested()

            onOpenProjectRequested: function (projectPath, displayName) {
                root.openProjectRequested(projectPath, displayName)
            }

            onOpenCloudProjectRequested: function (projectId, projectPath, displayName) {
                root.openCloudProjectRequested(projectId, projectPath, displayName)
            }
        }
    }

    Component {
        id: listComp

        ProjectsListView {
            id: list

            readonly property int nameColumnWidth: 100 + thumbnailWidth + nameSpacing
            readonly property int iconColumnWidth: 48
            readonly property int modifiedColumnWidth: 100
            readonly property int sizeColumnWidth: 75
            readonly property int btnColumnWidth: 44

            readonly property int thumbnailWidth: 90
            readonly property int thumbnailHeight: 48

            readonly property int nameSpacing: 24

            anchors.fill: parent

            model: root.model
            searchText: root.searchText

            backgroundColor: root.backgroundColor
            sideMargin: root.sideMargin

            showNewProjectItem: root.showNewProjectItem

            navigation.section: root.navigationSection
            navigation.order: root.navigationOrder
            navigation.name: root.navigationName + "List"
            navigation.accessible.name: root.listAccessibleName

            onCreateNewProjectRequested: root.createNewProjectRequested()

            onOpenProjectRequested: function (projectPath, displayName) {
                root.openProjectRequested(projectPath, displayName)
            }

            onOpenCloudProjectRequested: function (projectId, projectPath, displayName) {
                root.openCloudProjectRequested(projectId, projectPath, displayName)
            }

            columns: [
                ProjectsListView.ColumnItem {
                    id: nameColumn

                    header: qsTrc("project", "Name")

                    width: nameColumnWidth

                    delegate: Row {
                        height: parent.height
                        width: parent.width

                        ProjectThumbnail {
                            id: thumbnail

                            height: list.thumbnailHeight
                            width: list.thumbnailWidth

                            anchors.verticalCenter: parent.verticalCenter

                            path: item.thumbnailUrl ?? ""
                            placeholder: root.placeholder

                            backgroundColor: ui.theme.backgroundSecondaryColor
                            lineColor: Qt.alpha(ui.theme.fontPrimaryColor, 0.8)
                            borderColor: ui.theme.strokeColor
                        }

                        Item {
                            height: parent.height
                            width: nameSpacing
                        }

                        StyledTextLabel {
                            id: nameLabel

                            anchors.verticalCenter: parent.verticalCenter

                            width: parent.width - thumbnail.width - nameSpacing

                            text: item.name ?? ""
                            font: ui.theme.largeBodyFont
                            verticalAlignment: Text.AlignVCenter
                            horizontalAlignment: Text.AlignLeft
                            elide: Text.ElideRight

                            NavigationFocusBorder {
                                navigationCtrl: NavigationControl {
                                    name: "NameLabel"
                                    panel: navigationPanel
                                    row: navigationRow
                                    column: navigationColumnStart
                                    enabled: nameLabel.visible && nameLabel.enabled && !nameLabel.isEmpty
                                    accessible.name: nameColumn.header + ": " + nameLabel.text
                                    accessible.role: MUAccessible.StaticText

                                    onActiveChanged: function (active) {
                                        if (active) {
                                            listItem.scrollIntoView()
                                        }
                                    }
                                }
                            }
                        }
                    }
                },
                ProjectsListView.ColumnItem {
                    id: cloudIndicatorColumn

                    header: ""

                    width: iconColumnWidth

                    delegate: Item {
                        readonly property bool isCloud: root.cloudIndicatorAlwaysVisible || (item.isCloud ?? false)

                        width: parent.width
                        height: parent.height

                        Loader {
                            active: isCloud
                            anchors.centerIn: parent

                            sourceComponent: CloudProjectIndicatorButton {
                                mouseArea.enabled: false
                                isProgress: false
                                isDownloadedAndUpToDate: true

                                NavigationFocusBorder {
                                    navigationCtrl: NavigationControl {
                                        name: "CloudIndicator"
                                        panel: navigationPanel
                                        row: navigationRow
                                        column: navigationColumnStart
                                        enabled: isCloud
                                        accessible.name: qsTrc("project", "Cloud project indicator")
                                        accessible.role: MUAccessible.Information

                                        onActiveChanged: function (active) {
                                            if (active) {
                                                listItem.scrollIntoView()
                                                listItem.scrollColumnIntoView(remainingColumnIndex)
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                },
                ProjectsListView.ColumnItem {
                    id: modifiedColumn

                    header: qsTrc("project", "Modified")

                    width: modifiedColumnWidth

                    delegate: StyledTextLabel {
                        id: modifiedLabel
                        text: item.timeSinceModified ?? ""

                        font.capitalization: Font.AllUppercase
                        horizontalAlignment: Text.AlignLeft

                        NavigationFocusBorder {
                            navigationCtrl: NavigationControl {
                                name: "ModifiedLabel"
                                panel: navigationPanel
                                row: navigationRow
                                column: navigationColumnStart
                                enabled: modifiedLabel.visible && modifiedLabel.enabled && !modifiedLabel.isEmpty
                                accessible.name: modifiedColumn.header + ": " + modifiedLabel.text
                                accessible.role: MUAccessible.StaticText

                                onActiveChanged: function (active) {
                                    if (active) {
                                        listItem.scrollIntoView()
                                        listItem.scrollColumnIntoView(remainingColumnIndex)
                                    }
                                }
                            }

                            anchors.margins: -radius
                            radius: 2 + border.width
                        }
                    }
                },
                ProjectsListView.ColumnItem {
                    id: sizeColumn

                    header: qsTrc("global", "Size", "file size")

                    width: sizeColumnWidth

                    delegate: StyledTextLabel {
                        id: sizeLabel
                        text: Boolean(item.fileSize) ? item.fileSize : "-"

                        font: ui.theme.largeBodyFont
                        horizontalAlignment: Text.AlignLeft

                        NavigationFocusBorder {
                            navigationCtrl: NavigationControl {
                                name: "SizeLabel"
                                panel: navigationPanel
                                row: navigationRow
                                column: navigationColumnStart
                                enabled: sizeLabel.visible && sizeLabel.enabled && !sizeLabel.isEmpty
                                accessible.name: sizeColumn.header + ": " + (Boolean(item.fileSize) ? item.fileSize : qsTrc("global", "Unknown"))
                                accessible.role: MUAccessible.StaticText

                                onActiveChanged: function (active) {
                                    if (active) {
                                        listItem.scrollIntoView()
                                        listItem.scrollColumnIntoView(remainingColumnIndex)
                                    }
                                }
                            }

                            anchors.margins: -radius
                            radius: 2 + border.width
                        }
                    }
                },
                ProjectsListView.ColumnItem {
                    id: btnColumn

                    header: ""

                    width: btnColumnWidth

                    delegate: Item {
                        width: parent.width
                        height: 28

                        MenuButton {
                            id: menuButton

                            visible: Boolean(item.contextMenuModel)

                            width: 28
                            height: 28

                            anchors.centerIn: parent

                            menuModel: item.contextMenuModel

                            onHandleMenuItem: function (itemId) {
                                Qt.callLater(item.contextMenuModel.handleMenuItem, itemId)
                            }

                            Component.onCompleted: {
                                if (menuModel != null) {
                                    menuModel.load()
                                }
                            }

                            onMenuModelChanged: {
                                if (menuModel != null) {
                                    menuModel.load()
                                }
                            }

                            Rectangle {
                                anchors.fill: parent
                                color: "transparent"
                                radius: 3
                                border.width: 1
                                border.color: ui.theme.strokeColor
                            }

                            NavigationFocusBorder {
                                navigationCtrl: NavigationControl {
                                    name: "MenuButton"
                                    panel: navigationPanel
                                    row: navigationRow
                                    column: navigationColumnStart
                                    enabled: menuButton.visible && menuButton.enabled
                                    accessible.name: qsTrc("project", "Project item menu")
                                    accessible.role: MUAccessible.Button

                                    onActiveChanged: function (active) {
                                        if (active) {
                                            listItem.scrollIntoView()
                                            listItem.scrollColumnIntoView(remainingColumnIndex)
                                        }
                                    }

                                    onTriggered: {
                                        menuButton.clicked(null)
                                    }
                                }
                            }
                        }
                    }
                }
            ]
        }
    }
}
