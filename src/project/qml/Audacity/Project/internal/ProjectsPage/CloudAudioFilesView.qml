/*
* Audacity: A Digital Audio Editor
*/

import QtQuick
import QtQuick.Layouts
import QtQuick.Controls

import Muse.Ui 1.0
import Muse.UiComponents

import Audacity.Project 1.0
import Audacity.Cloud 1.0

import "../../."

ProjectsView {
    id: root

    function refresh() {
        if (!accountModel.isAuthorized) {
            return
        }

        cloudAudioFilesModel.reload()
        prv.updateDesiredRowCount()
    }

    AccountModel {
        id: accountModel

        onIsAuthorizedChanged: {
            if (accountModel.isAuthorized) {
                cloudAudioFilesModel.load()
            } else {
                cloudAudioFilesModel.clear()
            }
        }
    }

    CloudAudioFilesModel {
        id: cloudAudioFilesModel

        onStateChanged: {
            if (cloudAudioFilesModel.state === CloudAudioFilesModel.Fine) {
                prv.updateDesiredRowCount()
            }
        }
    }

    Component.onCompleted: {
        accountModel.init()

        if (accountModel.isAuthorized) {
            cloudAudioFilesModel.load()
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
        property string placeholderFile: ":/resources/AudioFilePlaceholder.svg"
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

            let totalDataRows = Math.ceil(cloudAudioFilesModel.rowCount / columns)
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

            if (isSatisfied || !cloudAudioFilesModel.hasMore) {
                return
            }

            updateDesiredRowCountScheduled = true

            Qt.callLater(function () {
                let view = activeView ? activeView.view : null
                let columns = view ? (view.columns || 1) : 1

                let rowsToAdd = Math.max(3 - remainingFullRowsBelowViewport, 1)
                let newDesiredRowCount = cloudAudioFilesModel.rowCount + rowsToAdd * columns

                if (cloudAudioFilesModel.desiredRowCount < newDesiredRowCount) {
                    cloudAudioFilesModel.desiredRowCount = newDesiredRowCount
                }

                updateDesiredRowCountScheduled = false
            })
        }
    }

    sourceComponent: {
        if (!accountModel.isAuthorized) {
            return notSignedInComp
        }

        switch (cloudAudioFilesModel.state) {
        case CloudAudioFilesModel.Error:
            return errorComp
        case CloudAudioFilesModel.Loading:
            return loadingComp
        case CloudAudioFilesModel.Fine:
            break
        }

        if (cloudAudioFilesModel.rowCount == 0 && !cloudAudioFilesModel.hasMore && cloudAudioFilesModel.state != CloudAudioFilesModel.Loading) {
            return emptyComp
        }

        return root.viewType === ProjectsPageModel.List ? listComp : gridComp
    }

    Component {
        id: gridComp

        ProjectsGridView {
            id: gridView

            anchors.fill: parent

            model: cloudAudioFilesModel
            searchText: root.searchText

            backgroundColor: root.backgroundColor
            sideMargin: root.sideMargin
            placeholder: prv.placeholderFile

            navigation.section: root.navigationSection
            navigation.order: root.navigationOrder
            navigation.name: "CloudAudioFilesGrid"
            navigation.accessible.name: qsTrc("project", "Cloud audio files grid")

            onOpenCloudProjectRequested: function (id, _slug, _path) {
                root.openCloudAudioFileRequested(id)
            }
        }
    }

    Component {
        id: listComp

        ProjectsListView {
            id: list

            readonly property int nameColumnWidth: nameLabelWidth
            readonly property int modifiedColumnWidth: 100
            readonly property int durationColumnWidth: 100
            readonly property int sizeColumnWidth: 75
            readonly property int btnColumnWidth: 44

            readonly property int nameLabelWidth: 200
            readonly property int thumbnailMinimumWidth: 250
            readonly property int thumbnailHeight: 48

            anchors.fill: parent

            model: cloudAudioFilesModel
            searchText: root.searchText

            backgroundColor: root.backgroundColor
            sideMargin: root.sideMargin

            navigation.section: root.navigationSection
            navigation.order: root.navigationOrder
            navigation.name: "CloudAudioFilesList"

            onOpenCloudProjectRequested: function (id, _slug, _path) {
                root.openCloudAudioFileRequested(id)
            }

            columns: [
                ProjectsListView.ColumnItem {
                    id: nameColumn

                    width: nameColumnWidth

                    header: qsTrc("project", "Name")

                    delegate: RowLayout {
                        anchors.fill: parent

                        StyledTextLabel {
                            id: nameLabel

                            Layout.preferredWidth: nameLabelWidth
                            Layout.minimumWidth: nameLabelWidth
                            Layout.fillWidth: !thumbnailItem.visible

                            text: item.name ?? ""
                            font: ui.theme.largeBodyFont
                            horizontalAlignment: Text.AlignLeft

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

                        Item {
                            id: thumbnailItem

                            visible: parent.width > (nameLabelWidth + thumbnailMinimumWidth)
                            Layout.fillHeight: true
                            Layout.fillWidth: true

                            ProjectThumbnail {
                                anchors.centerIn: parent
                                width: parent.width
                                height: thumbnailHeight

                                path: item ? item.thumbnailUrl : ""
                                placeholder: prv.placeholderFile

                                backgroundColor: "transparent"
                                lineColor: Qt.alpha(ui.theme.fontPrimaryColor, 0.8)
                                borderColor: "transparent"
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
                    id: durationColumn

                    header: qsTrc("global", "Duration", "file duration")

                    width: durationColumnWidth

                    delegate: StyledTextLabel {
                        id: durationLabel
                        text: Boolean(item.duration) ? item.duration : "-"

                        font: ui.theme.largeBodyFont
                        horizontalAlignment: Text.AlignLeft

                        NavigationFocusBorder {
                            navigationCtrl: NavigationControl {
                                name: "DurationLabel"
                                panel: navigationPanel
                                row: navigationRow
                                column: navigationColumnStart
                                enabled: durationLabel.visible && durationLabel.enabled && !durationLabel.isEmpty
                                accessible.name: durationColumn.header + ": " + (Boolean(item.duration) ? item.duration : qsTrc("global", "Unknown"))
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
                                item.contextMenuModel.handleMenuItem(itemId)
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

                title: qsTrc("project", "Unable to load online files")
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

                title: qsTrc("project", "You don’t have any online files yet")
                body: qsTrc("project", "Files will appear here when you save a file to the cloud, or publish a project")
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
                    body: qsTrc("project", "Please sign in to view your online files")
                }

                Row {
                    anchors.horizontalCenter: parent.horizontalCenter
                    width: implicitWidth
                    spacing: 12

                    NavigationPanel {
                        id: navPanel
                        name: "CloudAudioFilesSignInButtons"
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
