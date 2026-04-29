/*
* Audacity: A Digital Audio Editor
*/

import QtQuick 2.15
import QtQuick.Controls 2.15

import Muse.Ui 1.0
import Muse.UiComponents

import Audacity.Project 1.0
import Audacity.Cloud 1.0

ProjectsView {
    id: root

    function refresh() {
        if (!accountModel.isAuthorized) {
            return
        }

        cloudProjectsModel.reload()
        prv.updateDesiredRowCount()
    }

    AccountModel {
        id: accountModel

        onIsAuthorizedChanged: {
            if (accountModel.isAuthorized) {
                cloudProjectsModel.load()
            } else {
                cloudProjectsModel.clear()
            }
        }
    }

    CloudProjectsModel {
        id: cloudProjectsModel

        onStateChanged: {
            if (cloudProjectsModel.state === CloudProjectsModel.Fine) {
                prv.updateDesiredRowCount()
            }
        }
    }

    Component.onCompleted: {
        accountModel.init()

        if (accountModel.isAuthorized) {
            cloudProjectsModel.load()
        }
    }

    Connections {
        target: (root.item && root.item.view) ? root.item.view : null

        function onContentYChanged() {
            prv.updateDesiredRowCount()
        }
    }

    QtObject {
        id: prv
        property bool updateDesiredRowCountScheduled: false

        readonly property var activeView: root.item

        readonly property int remainingFullRowsBelowViewport: {
            if (!activeView || !activeView.view) {
                return 0
            }

            let view = activeView.view
            let columns = view.columns || 1
            let cellHeight = view.cellHeight || 100
            let topMargin = view.topMargin || 0

            let totalDataRows = Math.ceil(cloudProjectsModel.rowCount / columns)
            let scrolledContent = view.contentY + topMargin
            let currentScrollRow = Math.max(0, Math.floor(scrolledContent / cellHeight))
            let visibleRows = Math.ceil((view.height + (scrolledContent % cellHeight)) / cellHeight)
            let viewportBottomRow = currentScrollRow + visibleRows

            return Math.max(0, totalDataRows - viewportBottomRow)
        }

        readonly property bool isSatisfied: remainingFullRowsBelowViewport >= 2

        onIsSatisfiedChanged: {
            if (!isSatisfied) {
                updateDesiredRowCount()
            }
        }

        function updateDesiredRowCount() {
            if (updateDesiredRowCountScheduled) {
                return
            }

            if (isSatisfied || !cloudProjectsModel.hasMore) {
                return
            }

            updateDesiredRowCountScheduled = true

            Qt.callLater(function () {
                let view = activeView ? activeView.view : null
                let columns = view ? (view.columns || 1) : 1

                let rowsToAdd = Math.max(3 - remainingFullRowsBelowViewport, 1)
                let newDesiredRowCount = cloudProjectsModel.rowCount + rowsToAdd * columns

                if (cloudProjectsModel.desiredRowCount < newDesiredRowCount) {
                    cloudProjectsModel.desiredRowCount = newDesiredRowCount
                }

                updateDesiredRowCountScheduled = false
            })
        }
    }

    sourceComponent: {
        if (!accountModel.isAuthorized) {
            return notSignedInComp
        }

        switch (cloudProjectsModel.state) {
        case CloudProjectsModel.Error:
            return errorComp
        case CloudProjectsModel.Loading:
            return loadingComp
        case CloudProjectsModel.Fine:
            break
        }

        if (cloudProjectsModel.rowCount == 0 && !cloudProjectsModel.hasMore && cloudProjectsModel.state != CloudProjectsModel.Loading) {
            return emptyComp
        }

        return root.viewType === ProjectsPageModel.List ? listComp : gridComp
    }

    Component {
        id: gridComp

        ProjectsGridView {
            id: gridView

            anchors.fill: parent

            model: cloudProjectsModel
            searchText: root.searchText

            backgroundColor: root.backgroundColor
            sideMargin: root.sideMargin

            navigation.section: root.navigationSection
            navigation.order: root.navigationOrder
            navigation.name: "CloudProjectsGrid"
            navigation.accessible.name: qsTrc("project", "Cloud projects grid")

            onOpenCloudProjectRequested: function (cloudItemId, projectPath, displayName) {
                root.openCloudProjectRequested(cloudItemId, projectPath, displayName)
            }
        }
    }

    Component {
        id: listComp

        ProjectsListView {
            id: list

            anchors.fill: parent

            model: cloudProjectsModel
            searchText: root.searchText

            isCloudList: true

            backgroundColor: root.backgroundColor
            sideMargin: root.sideMargin

            navigation.section: root.navigationSection
            navigation.order: root.navigationOrder
            navigation.name: "CloudProjectsList"
            navigation.accessible.name: qsTrc("project", "Cloud projects list")

            onOpenCloudProjectRequested: function (cloudItemId, projectPath, displayName) {
                root.openCloudProjectRequested(cloudItemId, projectPath, displayName)
            }

            columns: [
                ProjectsListView.ColumnItem {
                    id: modifiedColumn

                    //: Stands for "Last time that this project was modified".
                    //: Used as the header of this column in the scores list.
                    header: qsTrc("project", "Modified")

                    width: function (parentWidth) {
                        let parentWidthExclusingSpacing = parentWidth - list.columns.length * list.view.columnSpacing
                        return 0.25 * parentWidthExclusingSpacing
                    }

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

                                onActiveChanged: {
                                    if (active) {
                                        listItem.scrollIntoView()
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

                    width: function (parentWidth) {
                        let parentWidthExclusingSpacing = parentWidth - list.columns.length * list.view.columnSpacing
                        return 0.15 * parentWidthExclusingSpacing
                    }

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

                                onActiveChanged: {
                                    if (active) {
                                        listItem.scrollIntoView()
                                    }
                                }
                            }

                            anchors.margins: -radius
                            radius: 2 + border.width
                        }
                    }
                }
            ]
        }
    }

    Component {
        id: errorComp

        Item {
            anchors.fill: parent

            Message {
                anchors.top: parent.top
                anchors.topMargin: Math.max(parent.height / 3 - height / 2, 0)
                anchors.left: parent.left
                anchors.leftMargin: root.sideMargin
                anchors.right: parent.right
                anchors.rightMargin: root.sideMargin

                title: qsTrc("project", "Unable to load online projects")
                body: qsTrc("global", "Please check your internet connection or try again later.")
            }
        }
    }

    Component {
        id: emptyComp

        Item {
            anchors.fill: parent

            Message {
                anchors.top: parent.top
                anchors.topMargin: Math.max(parent.height / 3 - height / 2, 0)
                anchors.left: parent.left
                anchors.leftMargin: root.sideMargin
                anchors.right: parent.right
                anchors.rightMargin: root.sideMargin

                title: qsTrc("project", "You don’t have any online projects yet")
                body: qsTrc("project", "Projects will appear here when you publish a project")
            }
        }
    }

    Component {
        id: notSignedInComp

        Item {
            anchors.fill: parent

            Column {
                anchors.top: parent.top
                anchors.topMargin: Math.max(parent.height / 3 - height / 2, 0)
                anchors.left: parent.left
                anchors.leftMargin: root.sideMargin
                anchors.right: parent.right
                anchors.rightMargin: root.sideMargin

                spacing: 32

                Message {
                    width: parent.width

                    title: qsTrc("project", "You are not signed in")
                    body: qsTrc("project", "Please sign in to view your online projects")
                }

                Row {
                    anchors.horizontalCenter: parent.horizontalCenter
                    width: implicitWidth
                    spacing: 12

                    NavigationPanel {
                        id: navPanel
                        name: "ProjectSignInButtons"
                        section: root.navigationSection
                        order: root.navigationOrder
                        direction: NavigationPanel.Horizontal
                        accessible.name: qsTrc("cloud", "Sign in buttons")
                    }

                    FlatButton {
                        navigation.panel: navPanel
                        navigation.order: 1

                        text: qsTrc("cloud", "Sign in")
                        onClicked: {
                            Qt.callLater(accountModel.openSignInDialog)
                        }
                    }

                    FlatButton {
                        navigation.panel: navPanel
                        navigation.order: 2

                        text: qsTrc("cloud", "Create account")
                        onClicked: {
                            Qt.callLater(accountModel.openCreateAccountDialog)
                        }
                    }
                }
            }
        }
    }

    Component {
        id: loadingComp

        Item {
            anchors.fill: parent

            StyledBusyIndicator {
                anchors.centerIn: parent
                width: 40
                height: 40
                running: true
            }
        }
    }
}
