/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick
import QtQuick.Layouts
import QtQuick.Window

import Muse.Ui
import Muse.UiComponents
import Muse.Dock

import Audacity.AIStudio

DockPage {
    id: root

    objectName: "Library"
    uri: "audacity://library"

    property var window: null

    property string filterKind: "all"
    property string libraryScope: "project"
    readonly property bool isProjectScope: libraryScope === "project"
    readonly property var currentAssets: isProjectScope ? AIStudioStatus.libraryAssets : AIStudioStatus.globalLibraryAssets
    property string searchText: ""
    property string sortMode: "name-ascending"
    property string selectedAssetId: ""
    property var selectedAssetIds: []
    property string selectionAnchorAssetId: ""
    property string contextAssetId: ""
    property string pendingRenameAssetId: ""
    property string pendingDeleteAssetId: ""
    property string moveFolderTarget: ""
    property var moveAssetIds: []
    readonly property string selectedLibraryFolder: filterKind.indexOf("folder:") === 0
                                                 ? filterKind.substring("folder:".length) : ""
    property var libraryViewEntries: [
            { key: "all", label: qsTrc("aistudio", "All assets") },
            { key: "favourites", label: qsTrc("aistudio", "Favourites") },
            { key: "imports", label: qsTrc("aistudio", "Imported WAV") },
            { key: "generated", label: qsTrc("aistudio", "Generated") },
            { key: "missing", label: qsTrc("aistudio", "Missing files") },
            { key: "unfiled", label: qsTrc("aistudio", "Unfiled") }
    ]
    property var projectFolderEntries: AIStudioStatus.libraryFolders.map(function(folder) {
        return { key: "folder:" + folder, label: folder }
    })
    function assetCountForView(viewKey) {
        const assets = currentAssets
        let count = 0
        for (let index = 0; index < assets.length; ++index) {
            const asset = assets[index]
            if (viewKey === "all"
                || (viewKey === "favourites" && asset.favourite)
                || (viewKey === "imports" && asset.origin === "uploaded")
                || (viewKey === "generated" && asset.origin === "generated")
                || (viewKey === "missing" && asset.status === "missing")
                || (viewKey === "unfiled" && !asset.folder)
                || (viewKey.indexOf("folder:") === 0
                    && asset.folder === viewKey.substring("folder:".length))) {
                ++count
            }
        }
        return count
    }
    function formatDuration(seconds) {
        if (!seconds || seconds <= 0) {
            return qsTrc("aistudio", "Unknown")
        }
        const totalSeconds = Math.round(seconds)
        const minutes = Math.floor(totalSeconds / 60)
        return "%1:%2".arg(minutes).arg(totalSeconds % 60 < 10 ? "0" + (totalSeconds % 60) : totalSeconds % 60)
    }
    function openMoveFolderForAssets(assetIds) {
        moveAssetIds = assetIds.slice()
        moveFolderTarget = assetIds.length === 1 && contextAsset ? contextAsset.folder || "" : ""
        moveFolderNewName.clear()
        moveFolderPanel.open()
    }
    property var displayedAssets: {
        const assets = currentAssets
        const result = []
        const query = searchText.toLowerCase()
        for (let index = 0; index < assets.length; ++index) {
            const asset = assets[index]
            const matchesFolder = filterKind === "all"
                                  || (filterKind === "imports" && asset.origin === "uploaded")
                                  || (filterKind === "generated" && asset.origin === "generated")
                                  || (filterKind === "missing" && asset.status === "missing")
                                  || (filterKind === "favourites" && asset.favourite)
                                  || (filterKind === "unfiled" && !asset.folder)
                                  || (filterKind.indexOf("folder:") === 0
                                      && asset.folder === filterKind.substring("folder:".length))
            const searchableText = (asset.name + " " + asset.kind + " " + asset.origin + " "
                                    + (asset.folder || "") + " " + (asset.createdAt || "") + " "
                                    + (asset.tags || []).join(" ")).toLowerCase()
            if (matchesFolder && (!query || searchableText.indexOf(query) !== -1)) {
                result.push(asset)
            }
        }
        result.sort(function(left, right) {
            if (sortMode === "newest") {
                return right.createdAt.localeCompare(left.createdAt)
            }
            if (sortMode === "duration-longest") {
                return right.durationSeconds - left.durationSeconds
            }
            if (sortMode === "duration-shortest") {
                const leftDuration = left.durationSeconds > 0 ? left.durationSeconds : Number.MAX_SAFE_INTEGER
                const rightDuration = right.durationSeconds > 0 ? right.durationSeconds : Number.MAX_SAFE_INTEGER
                return leftDuration - rightDuration
            }
            const comparison = left.name.toLowerCase().localeCompare(right.name.toLowerCase())
            return sortMode === "name-descending" ? -comparison : comparison
        })
        return result
    }
    property var selectedAsset: {
        for (let index = 0; index < displayedAssets.length; ++index) {
            if (displayedAssets[index].id === selectedAssetId) {
                return displayedAssets[index]
            }
        }
        return null
    }
    property var contextAsset: {
        for (let index = 0; index < currentAssets.length; ++index) {
            if (currentAssets[index].id === contextAssetId) {
                return currentAssets[index]
            }
        }
        return null
    }

    function importWav() {
        const sourcePaths = wavPicker.selectFiles()
        for (let index = 0; index < sourcePaths.length; ++index) {
            AIStudioStatus.importLocalWav(sourcePaths[index])
        }
    }

    function selectAsset(assetId, additive, range) {
        const anchorIndex = displayedAssets.findIndex(function(asset) {
            return asset.id === selectionAnchorAssetId
        })
        const clickedIndex = displayedAssets.findIndex(function(asset) {
            return asset.id === assetId
        })

        if (range && anchorIndex !== -1 && clickedIndex !== -1) {
            let selection = additive ? selectedAssetIds.slice() : []
            const first = Math.min(anchorIndex, clickedIndex)
            const last = Math.max(anchorIndex, clickedIndex)
            for (let index = first; index <= last; ++index) {
                const rangedAssetId = displayedAssets[index].id
                if (selection.indexOf(rangedAssetId) === -1) {
                    selection.push(rangedAssetId)
                }
            }
            selectedAssetIds = selection
            selectedAssetId = assetId
            return
        }

        let selection = additive ? selectedAssetIds.slice() : []
        const currentIndex = selection.indexOf(assetId)
        if (currentIndex === -1) {
            selection.push(assetId)
        } else if (additive) {
            selection.splice(currentIndex, 1)
        }
        selectedAssetIds = selection
        selectedAssetId = selection.indexOf(assetId) !== -1 ? assetId : (selection.length ? selection[0] : "")
        selectionAnchorAssetId = assetId
    }

    function addSelectedAssetsToTimeline() {
        for (let index = 0; index < selectedAssetIds.length; ++index) {
            AIStudioStatus.addLibraryAssetToTimeline(selectedAssetIds[index])
        }
    }

    function clearSelection() {
        selectedAssetId = ""
        selectedAssetIds = []
        selectionAnchorAssetId = ""
    }

    function selectDisplayedAssets() {
        const ids = []
        for (let index = 0; index < displayedAssets.length; ++index) {
            ids.push(displayedAssets[index].id)
        }
        selectedAssetIds = ids
        selectedAssetId = ids.length ? ids[0] : ""
        selectionAnchorAssetId = selectedAssetId
    }

    function selectedAssetsAreAllFavourite() {
        if (selectedAssetIds.length === 0) {
            return false
        }
        for (let selectedIndex = 0; selectedIndex < selectedAssetIds.length; ++selectedIndex) {
            const assetId = selectedAssetIds[selectedIndex]
            const asset = currentAssets.find(function(candidate) {
                return candidate.id === assetId
            })
            if (!asset || !asset.favourite) {
                return false
            }
        }
        return true
    }

    function maximizeAppWindow() {
        if (window && window.visibility !== Window.Maximized) {
            window.showMaximized()
        }
    }

    onVisibleChanged: {
        if (visible) {
            Qt.callLater(maximizeAppWindow)
            AIStudioStatus.refreshLibrary()
        }
    }

    onWindowChanged: {
        if (visible) {
            Qt.callLater(maximizeAppWindow)
        }
    }

    FilePickerModel {
        id: wavPicker
        title: qsTrc("aistudio", "Import WAV files into AI Library")
        filter: [qsTrc("aistudio", "WAV audio (*.wav)")]
    }

    central: Rectangle {
        id: libraryCentral
        color: ui.theme.backgroundPrimaryColor

        ColumnLayout {
            anchors.fill: parent
            anchors.margins: 24
            spacing: 12

            RowLayout {
                Layout.fillWidth: true

                ColumnLayout {
                    Layout.fillWidth: true
                    spacing: 3

                    StyledTextLabel {
                        text: root.isProjectScope ? qsTrc("aistudio", "Library — This Project")
                                                  : qsTrc("aistudio", "Library — All Projects")
                        font: ui.theme.headerBoldFont
                    }
                    StyledTextLabel {
                        text: root.isProjectScope ? qsTrc("aistudio", "This Project  /  AI Library")
                                                  : qsTrc("aistudio", "All indexed projects  /  AI Library")
                    }
                }

                StyledDropdown {
                    Layout.preferredWidth: 155
                    model: [
                        { text: qsTrc("aistudio", "This Project"), value: "project" },
                        { text: qsTrc("aistudio", "All Projects"), value: "global" }
                    ]
                    currentIndex: root.isProjectScope ? 0 : 1
                    onActivated: function(index, value) {
                        root.libraryScope = value
                        root.filterKind = "all"
                        root.clearSelection()
                    }
                }

                FlatButton {
                    text: qsTrc("aistudio", "Refresh")
                    onClicked: AIStudioStatus.refreshLibrary()
                }

                FlatButton {
                    text: qsTrc("aistudio", "Import local WAV")
                    enabled: root.isProjectScope
                    onClicked: root.importWav()
                }
            }

            RowLayout {
                Layout.fillWidth: true
                spacing: 10

                SearchField {
                    id: assetSearch
                    Layout.fillWidth: true
                    onSearchTextChanged: root.searchText = searchText
                }

                StyledDropdown {
                    id: sortDropdown
                    Layout.preferredWidth: 190
                    model: [
                        { text: qsTrc("aistudio", "Name, A–Z"), value: "name-ascending" },
                        { text: qsTrc("aistudio", "Name, Z–A"), value: "name-descending" },
                        { text: qsTrc("aistudio", "Newest first"), value: "newest" },
                        { text: qsTrc("aistudio", "Duration, longest"), value: "duration-longest" },
                        { text: qsTrc("aistudio", "Duration, shortest"), value: "duration-shortest" }
                    ]
                    currentIndex: 0
                    onActivated: function(index, value) {
                        root.sortMode = value
                    }
                }
            }

            StyledTextLabel {
                Layout.fillWidth: true
                wrapMode: Text.Wrap
                text: qsTrc("aistudio", "Library activity: %1").arg(AIStudioStatus.libraryStatus)
                font: ui.theme.bodyBoldFont
            }

            Rectangle { Layout.fillWidth: true; Layout.preferredHeight: 1; color: ui.theme.strokeColor }

            RowLayout {
                Layout.fillWidth: true
                Layout.fillHeight: true
                spacing: 0

                Rectangle {
                    Layout.fillHeight: true
                    Layout.preferredWidth: 176
                    color: ui.theme.backgroundSecondaryColor

                    ColumnLayout {
                        anchors.fill: parent
                        anchors.margins: 12
                        spacing: 5

                        StyledTextLabel {
                            Layout.fillWidth: true
                            text: qsTrc("aistudio", "LIBRARY VIEWS")
                            font: ui.theme.bodyBoldFont
                        }

                        Repeater {
                            model: root.libraryViewEntries

                            delegate: FlatButton {
                                Layout.fillWidth: true
                                text: "%1 (%2)".arg(modelData.label).arg(root.assetCountForView(modelData.key))
                                enabled: root.filterKind !== modelData.key
                                onClicked: {
                                    root.filterKind = modelData.key
                                    root.clearSelection()
                                }
                            }
                        }

                        Rectangle { Layout.fillWidth: true; Layout.preferredHeight: 1; color: ui.theme.strokeColor }

                        StyledTextLabel {
                            Layout.fillWidth: true
                            text: qsTrc("aistudio", "PROJECT FOLDERS")
                            font: ui.theme.bodyBoldFont
                        }

                        StyledTextLabel {
                            Layout.fillWidth: true
                            wrapMode: Text.Wrap
                            text: root.isProjectScope
                                  ? qsTrc("aistudio", "Create folders here, then move selected assets into them.")
                                  : qsTrc("aistudio", "Project folders are edited from This Project.")
                        }

                        TextInputField {
                            id: folderNameField
                            Layout.fillWidth: true
                            hint: root.selectedLibraryFolder
                                  ? qsTrc("aistudio", "New folder name")
                                  : qsTrc("aistudio", "Folder name")
                        }

                        RowLayout {
                            Layout.fillWidth: true
                            spacing: 4

                            FlatButton {
                                Layout.fillWidth: true
                                icon: IconCode.PLUS
                                text: qsTrc("aistudio", "New")
                                enabled: root.isProjectScope && folderNameField.inputField.text.trim().length > 0
                                onClicked: {
                                    AIStudioStatus.createLibraryFolder(folderNameField.inputField.text)
                                    folderNameField.clear()
                                }
                            }

                            FlatButton {
                                Layout.fillWidth: true
                                icon: IconCode.EDIT
                                text: qsTrc("aistudio", "Rename")
                                enabled: root.isProjectScope && root.selectedLibraryFolder.length > 0
                                         && folderNameField.inputField.text.trim().length > 0
                                onClicked: {
                                    AIStudioStatus.renameLibraryFolder(root.selectedLibraryFolder,
                                                                       folderNameField.inputField.text)
                                    root.filterKind = "all"
                                    folderNameField.clear()
                                    root.clearSelection()
                                }
                            }

                            FlatButton {
                                Layout.fillWidth: true
                                icon: IconCode.DELETE_TANK
                                text: qsTrc("aistudio", "Delete")
                                enabled: root.isProjectScope && root.selectedLibraryFolder.length > 0
                                onClicked: {
                                    AIStudioStatus.deleteLibraryFolder(root.selectedLibraryFolder)
                                    root.filterKind = "all"
                                    root.clearSelection()
                                }
                            }
                        }

                        Repeater {
                            model: root.projectFolderEntries

                            delegate: FlatButton {
                                Layout.fillWidth: true
                                icon: IconCode.OPEN_FILE
                                text: "%1 (%2)".arg(modelData.label).arg(root.assetCountForView(modelData.key))
                                enabled: root.filterKind !== modelData.key
                                onClicked: {
                                    root.filterKind = modelData.key
                                    root.clearSelection()
                                }
                            }
                        }

                        Item { Layout.fillHeight: true }
                    }
                }

                Rectangle { Layout.fillHeight: true; Layout.preferredWidth: 1; color: ui.theme.strokeColor }

                ColumnLayout {
                    Layout.fillWidth: true
                    Layout.fillHeight: true
                    Layout.leftMargin: 16
                    Layout.rightMargin: 16
                    spacing: 8

                    RowLayout {
                        Layout.fillWidth: true
                        StyledTextLabel { Layout.fillWidth: true; text: qsTrc("aistudio", "Name"); font: ui.theme.bodyBoldFont }
                        StyledTextLabel { Layout.preferredWidth: 120; text: qsTrc("aistudio", "Folder"); font: ui.theme.bodyBoldFont }
                        StyledTextLabel { Layout.preferredWidth: 80; text: qsTrc("aistudio", "Duration"); font: ui.theme.bodyBoldFont }
                        StyledTextLabel { Layout.preferredWidth: 140; text: qsTrc("aistudio", "Audio format"); font: ui.theme.bodyBoldFont }
                        StyledTextLabel { Layout.preferredWidth: 100; text: qsTrc("aistudio", "Status"); font: ui.theme.bodyBoldFont }
                    }

                    Rectangle { Layout.fillWidth: true; Layout.preferredHeight: 1; color: ui.theme.strokeColor }

                    ListView {
                        id: assetList
                        Layout.fillWidth: true
                        Layout.fillHeight: true
                        clip: true
                        spacing: 3
                        model: root.displayedAssets

                        delegate: Rectangle {
                            id: assetRow
                            required property var modelData
                            width: ListView.view.width
                            height: 42
                            radius: 3
                            color: root.selectedAssetIds.indexOf(modelData.id) !== -1 ? ui.theme.accentColor : "transparent"

                            RowLayout {
                                anchors.fill: parent
                                anchors.leftMargin: 8
                                anchors.rightMargin: 8
                                spacing: 10

                                StyledTextLabel { Layout.fillWidth: true; elide: Text.ElideRight; text: modelData.name }
                                StyledTextLabel { Layout.preferredWidth: 120; elide: Text.ElideRight; text: modelData.folder || qsTrc("aistudio", "Unfiled") }
                                StyledTextLabel { Layout.preferredWidth: 80; elide: Text.ElideRight; text: root.formatDuration(modelData.durationSeconds) }
                                StyledTextLabel {
                                    Layout.preferredWidth: 140
                                    elide: Text.ElideRight
                                    text: modelData.sampleRate > 0
                                          ? qsTrc("aistudio", "%1 Hz · %2 ch").arg(modelData.sampleRate).arg(modelData.channels)
                                          : qsTrc("aistudio", "Unknown")
                                }
                                StyledTextLabel { Layout.preferredWidth: 100; elide: Text.ElideRight; text: modelData.status }
                            }

                            MouseArea {
                                id: assetRowMouseArea
                                anchors.fill: parent
                                acceptedButtons: Qt.LeftButton | Qt.RightButton
                                onPressed: function(mouse) {
                                    if (mouse.button === Qt.LeftButton) {
                                        assetContextMenu.visible = false
                                    }
                                }
                                onClicked: function(mouse) {
                                    if (mouse.button !== Qt.RightButton) {
                                        root.selectAsset(modelData.id,
                                                         (mouse.modifiers & Qt.ControlModifier) !== 0,
                                                         (mouse.modifiers & Qt.ShiftModifier) !== 0)
                                        return
                                    }
                                    root.selectAsset(modelData.id, false, false)
                                    root.contextAssetId = modelData.id
                                    const menuPosition = assetRow.mapToItem(libraryCentral, mouse.x, mouse.y)
                                    assetContextMenu.openAt(menuPosition.x, menuPosition.y)
                                }
                            }
                        }

                        StyledTextLabel {
                            anchors.centerIn: parent
                            visible: assetList.count === 0
                            text: qsTrc("aistudio", "No assets in this folder")
                        }
                    }

                    RowLayout {
                        Layout.fillWidth: true

                        StyledTextLabel {
                            Layout.fillWidth: true
                            text: root.displayedAssets.length === 0
                                  ? qsTrc("aistudio", "No assets shown")
                                  : qsTrc("aistudio", "%1 assets shown").arg(root.displayedAssets.length)
                        }

                        FlatButton {
                            text: qsTrc("aistudio", "Select all shown")
                            enabled: root.displayedAssets.length > 0
                                     && root.selectedAssetIds.length !== root.displayedAssets.length
                            onClicked: root.selectDisplayedAssets()
                        }

                        FlatButton {
                            text: qsTrc("aistudio", "Clear selection")
                            enabled: root.selectedAssetIds.length > 0
                            onClicked: root.clearSelection()
                        }
                    }

                    RowLayout {
                        Layout.fillWidth: true

                        TextInputField {
                            id: tagsField
                            Layout.fillWidth: true
                            currentText: root.selectedAsset ? (root.selectedAsset.tags || []).join(", ") : ""
                            hint: qsTrc("aistudio", "Tags, separated by commas")
                        }

                        FlatButton {
                            text: root.selectedAssetIds.length > 1
                                  ? qsTrc("aistudio", "Save tags to %1").arg(root.selectedAssetIds.length)
                                  : qsTrc("aistudio", "Save tags")
                            enabled: root.isProjectScope && root.selectedAssetIds.length > 0
                            onClicked: AIStudioStatus.setLibraryAssetsTags(root.selectedAssetIds,
                                                                            tagsField.inputField.text)
                        }

                        FlatButton {
                            text: root.selectedAssetIds.length > 1
                                  ? qsTrc("aistudio", "Read details for %1").arg(root.selectedAssetIds.length)
                                  : qsTrc("aistudio", "Read WAV details")
                            enabled: root.isProjectScope && root.selectedAssetIds.length > 0
                            onClicked: AIStudioStatus.readLibraryAssetsAudioDetails(root.selectedAssetIds)
                        }
                    }

                    Rectangle { Layout.fillWidth: true; Layout.preferredHeight: 1; color: ui.theme.strokeColor }

                    Rectangle {
                        Layout.fillWidth: true
                        Layout.preferredHeight: detailsLayout.implicitHeight + 18
                        visible: root.selectedAsset
                        color: ui.theme.backgroundSecondaryColor
                        radius: 4

                        GridLayout {
                            id: detailsLayout
                            anchors.fill: parent
                            anchors.margins: 9
                            columns: 2
                            columnSpacing: 14
                            rowSpacing: 4

                            StyledTextLabel { text: qsTrc("aistudio", "Asset details"); font: ui.theme.bodyBoldFont }
                            Item { Layout.fillWidth: true }
                            StyledTextLabel { text: qsTrc("aistudio", "Folder") }
                            StyledTextLabel { Layout.fillWidth: true; elide: Text.ElideRight; text: root.selectedAsset.folder || qsTrc("aistudio", "Unfiled") }
                            StyledTextLabel { text: qsTrc("aistudio", "Tags") }
                            StyledTextLabel { Layout.fillWidth: true; elide: Text.ElideRight; text: (root.selectedAsset.tags || []).join(", ") || qsTrc("aistudio", "No tags") }
                            StyledTextLabel { text: qsTrc("aistudio", "Origin") }
                            StyledTextLabel { Layout.fillWidth: true; elide: Text.ElideRight; text: root.selectedAsset.origin }
                            StyledTextLabel { text: qsTrc("aistudio", "Project") }
                            StyledTextLabel {
                                Layout.fillWidth: true
                                elide: Text.ElideMiddle
                                text: root.selectedAsset.projectPath || qsTrc("aistudio", "This project")
                            }
                            StyledTextLabel { text: qsTrc("aistudio", "Lineage") }
                            StyledTextLabel {
                                Layout.fillWidth: true
                                elide: Text.ElideRight
                                text: root.selectedAsset.provenanceId
                                      ? qsTrc("aistudio", "Recorded (%1)").arg(root.selectedAsset.provenanceId)
                                      : (root.selectedAsset.sourceAssetIds || []).length > 0
                                        ? qsTrc("aistudio", "Derived from %1 asset(s)").arg(root.selectedAsset.sourceAssetIds.length)
                                        : qsTrc("aistudio", "Not recorded")
                            }
                            StyledTextLabel { text: qsTrc("aistudio", "Added (UTC)") }
                            StyledTextLabel { Layout.fillWidth: true; elide: Text.ElideRight; text: root.selectedAsset.createdAt || qsTrc("aistudio", "Unknown") }
                            StyledTextLabel { text: qsTrc("aistudio", "Duration") }
                            StyledTextLabel { Layout.fillWidth: true; text: root.formatDuration(root.selectedAsset.durationSeconds) }
                            StyledTextLabel { text: qsTrc("aistudio", "Audio format") }
                            StyledTextLabel {
                                Layout.fillWidth: true
                                text: root.selectedAsset.sampleRate > 0
                                      ? qsTrc("aistudio", "%1 Hz · %2 channel(s)").arg(root.selectedAsset.sampleRate).arg(root.selectedAsset.channels)
                                      : qsTrc("aistudio", "Unknown")
                            }
                            StyledTextLabel { text: qsTrc("aistudio", "Library path") }
                            StyledTextLabel { Layout.fillWidth: true; elide: Text.ElideRight; text: root.selectedAsset.filePath }
                        }
                    }

                    RowLayout {
                        Layout.fillWidth: true

                        StyledTextLabel {
                            Layout.fillWidth: true
                            elide: Text.ElideRight
                            text: root.selectedAssetIds.length > 1
                                  ? qsTrc("aistudio", "%1 assets selected").arg(root.selectedAssetIds.length)
                                  : root.selectedAsset
                                  ? qsTrc("aistudio", "%1  ·  %2  ·  %3").arg(root.selectedAsset.name).arg(root.selectedAsset.origin).arg(root.selectedAsset.status)
                                  : qsTrc("aistudio", "Select an asset, Ctrl-click to add, or Shift-click a range")
                        }

                        FlatButton {
                            text: root.selectedAssetIds.length > 1 && root.selectedAssetsAreAllFavourite()
                                  ? qsTrc("aistudio", "Unfavourite %1").arg(root.selectedAssetIds.length)
                                  : root.selectedAssetIds.length > 1
                                  ? qsTrc("aistudio", "Favourite %1").arg(root.selectedAssetIds.length)
                                  : root.selectedAsset && root.selectedAsset.favourite
                                  ? qsTrc("aistudio", "Unfavourite")
                                  : qsTrc("aistudio", "Favourite")
                            enabled: root.selectedAssetIds.length > 0
                            onClicked: {
                                const favourite = root.selectedAssetIds.length === 1
                                                  ? !root.selectedAsset.favourite
                                                  : !root.selectedAssetsAreAllFavourite()
                                AIStudioStatus.setLibraryAssetsFavourite(root.selectedAssetIds, favourite)
                            }
                        }

                        FlatButton {
                            text: root.selectedAssetIds.length > 1
                                  ? qsTrc("aistudio", "Add %1 to Timeline").arg(root.selectedAssetIds.length)
                                  : qsTrc("aistudio", "Add to Timeline")
                            enabled: root.selectedAssetIds.length > 0
                            onClicked: root.addSelectedAssetsToTimeline()
                        }
                    }

                    RowLayout {
                        Layout.fillWidth: true

                        TextInputField {
                            id: moveFolderNameField
                            Layout.fillWidth: true
                            hint: qsTrc("aistudio", "New or existing folder name")
                            onAccepted: moveToFolderButton.clicked()
                        }

                        FlatButton {
                            id: moveToFolderButton
                            text: root.selectedAssetIds.length > 1
                                  ? qsTrc("aistudio", "Move %1 assets").arg(root.selectedAssetIds.length)
                                  : qsTrc("aistudio", "Move to folder")
                            enabled: root.isProjectScope && root.selectedAssetIds.length > 0 && moveFolderNameField.inputField.text.trim().length > 0
                            onClicked: {
                                AIStudioStatus.moveLibraryAssetsToFolder(root.selectedAssetIds,
                                                                          moveFolderNameField.inputField.text)
                                moveFolderNameField.clear()
                            }
                        }

                        FlatButton {
                            text: qsTrc("aistudio", "Choose folder")
                            enabled: root.isProjectScope && root.selectedAssetIds.length > 0
                            onClicked: root.openMoveFolderForAssets(root.selectedAssetIds)
                        }
                    }

                    RowLayout {
                        Layout.fillWidth: true
                        visible: root.pendingRenameAssetId.length > 0

                        TextInputField {
                            id: assetRenameField
                            Layout.fillWidth: true
                            currentText: root.contextAsset ? root.contextAsset.name : ""
                            hint: qsTrc("aistudio", "Asset name")
                        }

                        FlatButton {
                            text: qsTrc("aistudio", "Rename asset")
                            enabled: assetRenameField.inputField.text.trim().length > 0
                            onClicked: {
                                AIStudioStatus.renameLibraryAsset(root.pendingRenameAssetId,
                                                                  assetRenameField.inputField.text)
                                root.pendingRenameAssetId = ""
                            }
                        }

                        FlatButton {
                            text: qsTrc("aistudio", "Cancel")
                            onClicked: root.pendingRenameAssetId = ""
                        }
                    }

                    RowLayout {
                        Layout.fillWidth: true
                        visible: root.pendingDeleteAssetId.length > 0

                        StyledTextLabel {
                            Layout.fillWidth: true
                            text: qsTrc("aistudio", "Remove %1 from this project's Library? The audio file will be kept on disk.")
                                  .arg(root.contextAsset ? root.contextAsset.name : "this asset")
                        }

                        FlatButton {
                            text: qsTrc("aistudio", "Remove")
                            icon: IconCode.DELETE_TANK
                            onClicked: {
                                AIStudioStatus.deleteLibraryAsset(root.pendingDeleteAssetId)
                                root.pendingDeleteAssetId = ""
                                root.clearSelection()
                            }
                        }

                        FlatButton {
                            text: qsTrc("aistudio", "Cancel")
                            onClicked: root.pendingDeleteAssetId = ""
                        }
                    }
                }
            }

            Rectangle {
                id: moveFolderPanel
                visible: false
                width: 440
                height: 300
                anchors.centerIn: parent
                z: 10
                color: ui.theme.popupBackgroundColor
                border.width: 1
                border.color: ui.theme.strokeColor
                radius: 6

                function open() {
                    visible = true
                }

                function close() {
                    visible = false
                }

                ColumnLayout {
                    anchors.fill: parent
                    anchors.margins: 16
                    spacing: 8

                    StyledTextLabel {
                        Layout.fillWidth: true
                        text: root.moveAssetIds.length > 1
                              ? qsTrc("aistudio", "Move %1 assets to folder").arg(root.moveAssetIds.length)
                              : qsTrc("aistudio", "Move %1 to folder")
                                    .arg(root.contextAsset ? root.contextAsset.name : "asset")
                        font: ui.theme.bodyBoldFont
                    }

                    StyledTextLabel {
                        Layout.fillWidth: true
                        text: qsTrc("aistudio", "Choose an existing project folder or enter a new one.")
                    }

                    ListView {
                        Layout.fillWidth: true
                        Layout.fillHeight: true
                        clip: true
                        spacing: 3
                        model: AIStudioStatus.libraryFolders

                        delegate: FlatButton {
                            width: ListView.view.width
                            text: modelData
                            icon: IconCode.OPEN_FILE
                            enabled: root.moveFolderTarget !== modelData
                            onClicked: root.moveFolderTarget = modelData
                        }
                    }

                    TextInputField {
                        id: moveFolderNewName
                        Layout.fillWidth: true
                        hint: qsTrc("aistudio", "Or enter a new folder name")
                        onTextEdited: root.moveFolderTarget = ""
                    }

                    RowLayout {
                        Layout.fillWidth: true

                        FlatButton {
                            text: qsTrc("aistudio", "Move to Unfiled")
                            enabled: root.moveAssetIds.length > 0
                            onClicked: {
                                AIStudioStatus.moveLibraryAssetsToUnfiled(root.moveAssetIds)
                                root.moveFolderTarget = ""
                                root.moveAssetIds = []
                                moveFolderNewName.clear()
                                moveFolderPanel.close()
                            }
                        }

                        FlatButton {
                            Layout.fillWidth: true
                            text: qsTrc("aistudio", "Move to folder")
                            enabled: root.moveAssetIds.length > 0 && (root.moveFolderTarget.length > 0
                                     || moveFolderNewName.inputField.text.trim().length > 0)
                            onClicked: {
                                const folder = root.moveFolderTarget.length > 0
                                               ? root.moveFolderTarget : moveFolderNewName.inputField.text
                                AIStudioStatus.moveLibraryAssetsToFolder(root.moveAssetIds, folder)
                                root.moveFolderTarget = ""
                                root.moveAssetIds = []
                                moveFolderNewName.clear()
                                moveFolderPanel.close()
                            }
                        }

                        FlatButton {
                            text: qsTrc("aistudio", "Cancel")
                            onClicked: {
                                root.moveAssetIds = []
                                moveFolderPanel.close()
                            }
                        }
                    }
                }
            }

        }

        Rectangle {
                id: assetContextMenu
                visible: false
                width: 210
                height: contextMenuLayout.implicitHeight + 12
                z: 20
                color: ui.theme.popupBackgroundColor
                border.width: 1
                border.color: ui.theme.strokeColor
                radius: 4

                function openAt(menuX, menuY) {
                    x = Math.min(Math.max(0, menuX), libraryCentral.width - width)
                    y = Math.min(Math.max(0, menuY), libraryCentral.height - height)
                    visible = true
                }

                ColumnLayout {
                    id: contextMenuLayout
                    anchors.fill: parent
                    anchors.margins: 6
                    spacing: 2

                    FlatButton {
                        Layout.fillWidth: true
                        text: qsTrc("aistudio", "Rename")
                        icon: IconCode.EDIT
                        enabled: root.isProjectScope
                        onClicked: {
                            assetContextMenu.visible = false
                            root.pendingRenameAssetId = root.contextAssetId
                        }
                    }

                    FlatButton {
                        Layout.fillWidth: true
                        visible: !root.isProjectScope
                        text: qsTrc("aistudio", "Copy to This Project")
                        icon: IconCode.PLUS
                        onClicked: {
                            assetContextMenu.visible = false
                            if (root.contextAsset) {
                                AIStudioStatus.copyGlobalLibraryAssetToProject(root.contextAsset.projectPath,
                                                                               root.contextAssetId)
                            }
                        }
                    }

                    FlatButton {
                        Layout.fillWidth: true
                        text: qsTrc("aistudio", "Move to folder")
                        icon: IconCode.OPEN_FILE
                        enabled: root.isProjectScope
                        onClicked: {
                            assetContextMenu.visible = false
                            root.openMoveFolderForAssets([root.contextAssetId])
                        }
                    }

                    FlatButton {
                        Layout.fillWidth: true
                        text: qsTrc("aistudio", "Add to Timeline")
                        icon: IconCode.PLUS
                        enabled: root.isProjectScope
                        onClicked: {
                            assetContextMenu.visible = false
                            AIStudioStatus.addLibraryAssetToTimeline(root.contextAssetId)
                        }
                    }

                    FlatButton {
                        Layout.fillWidth: true
                        text: qsTrc("aistudio", "Show in Explorer")
                        icon: IconCode.OPEN_FILE
                        enabled: root.isProjectScope
                        onClicked: AIStudioStatus.revealLibraryAssetInExplorer(root.contextAssetId)
                    }

                    FlatButton {
                        Layout.fillWidth: true
                        text: root.contextAsset && root.contextAsset.favourite
                              ? qsTrc("aistudio", "Unfavourite") : qsTrc("aistudio", "Favourite")
                        enabled: root.isProjectScope
                        onClicked: {
                            assetContextMenu.visible = false
                            if (root.contextAsset) {
                                AIStudioStatus.setLibraryAssetFavourite(root.contextAssetId,
                                                                        !root.contextAsset.favourite)
                            }
                        }
                    }

                    Rectangle { Layout.fillWidth: true; Layout.preferredHeight: 1; color: ui.theme.strokeColor }

                    FlatButton {
                        Layout.fillWidth: true
                        text: qsTrc("aistudio", "Remove from Project Library")
                        icon: IconCode.DELETE_TANK
                        enabled: root.isProjectScope
                        onClicked: {
                            assetContextMenu.visible = false
                            root.pendingDeleteAssetId = root.contextAssetId
                        }
                    }
                }
            }
    }

}
