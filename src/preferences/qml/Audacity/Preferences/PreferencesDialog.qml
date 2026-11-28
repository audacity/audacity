/*
 * SPDX-License-Identifier: GPL-3.0-only
 * Audacity-CLA-applies
 *
 * Audacity
 * A Digital Audio Editor
 *
 * Copyright (C) 2024 Audacity BVBA and others
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 3 as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
import QtQuick 2.15
import QtQuick.Layouts 1.15

import Muse.Ui 1.0
import Muse.UiComponents

StyledDialogView {
    id: root

    title: qsTrc("preferences", "Preferences")

    contentWidth: 880
    contentHeight: 640
    resizable: true

    property string currentPageId: ""
    property var params: null
    property string searchQuery: ""
    property var searchIndex: null
    property var searchMatches: []
    property Item searchHighlight: null
    closeOnEscape: !searchQuery

    function indexLabels(item, labels) {
        for (const child of (item.children || [])) {
            indexLabels(child, labels)
        }
        if (typeof item.title === "string") {
            labels.push({ text: item.title.toLocaleLowerCase(), item: item })
        }
        if (typeof item.text === "string" && typeof item.cursorPosition !== "number") {
            labels.push({ text: item.text.toLocaleLowerCase(), item: item })
        }
    }

    function updateSearch(text) {
        searchHighlight = null
        searchQuery = text.trim().toLocaleLowerCase()
        if (!searchQuery) {
            searchMatches = []
            return
        }
        if (searchIndex === null) {
            searchIndex = []
            for (const page of preferencesModel.availablePages()) {
                const item = prv.pagesObjects[page.id]
                if (item) {
                    const labels = []
                    indexLabels(item, labels)
                    searchIndex.push({ id: page.id, title: page.title, labels: labels })
                }
            }
        }
        const matches = []
        for (const page of searchIndex) {
            const label = page.labels.find(entry => entry.text.includes(searchQuery))
            if (label || page.title.toLocaleLowerCase().includes(searchQuery)) {
                matches.push({ id: page.id, title: page.title, item: label ? label.item : null })
            }
        }
        searchMatches = matches
        results.currentIndex = -1
    }

    function openSearchResult(index) {
        const result = searchMatches[index]
        if (!result) {
            return
        }
        preferencesModel.currentPageId = result.id
        Qt.callLater(function () {
            const item = result.item
            if (!item || !item.visible) {
                return
            }
            root.searchHighlight = item instanceof Text ? item : null
            for (let ancestor = item.parent; ancestor; ancestor = ancestor.parent) {
                if (ancestor instanceof Flickable) {
                    const y = item.mapToItem(ancestor.contentItem, 0, 0).y
                    ancestor.contentY = Math.max(ancestor.originY,
                                                 Math.min(y - 16, ancestor.originY + ancestor.contentHeight - ancestor.height))
                    break
                }
            }
        })
    }

    Shortcut {
        sequence: "Ctrl+Shift+L"
        context: Qt.ApplicationShortcut
        enabled: root.isOpened
        onActivated: {
            search.navigation.requestActive()
            search.ensureActiveFocus()
            search.selectAll()
        }
    }

    Connections {
        target: root.navigationSection
        function onNavigationEvent(event) {
            if (event.type === NavigationEvent.Escape && root.searchQuery) {
                event.accepted = true
                Qt.callLater(function () {
                    search.clear()
                    search.navigation.requestActive()
                })
            }
        }
    }

    Rectangle {
        parent: root.searchHighlight
        visible: root.searchHighlight !== null
        anchors.fill: parent
        anchors.margins: -2
        color: "transparent"
        border.color: ui.theme.accentColor
        radius: 3
        z: 1
    }

    property QtObject prv: QtObject {
        property var pagesObjects: (new Map())

        function resolveStackCurrentIndex() {
            var keys = Object.keys(root.prv.pagesObjects)
            return keys.indexOf(preferencesModel.currentPageId)
        }

        function updateStackCurrentIndex() {
            stack.currentIndex = resolveStackCurrentIndex()
        }
    }

    Component.onCompleted: {
        preferencesModel.load(root.currentPageId)

        initPagesObjects()

        prv.updateStackCurrentIndex()
    }

    function initPagesObjects() {
        var pages = preferencesModel.availablePages()
        for (var i in pages) {
            var pageInfo = pages[i]

            if (!Boolean(pageInfo.path)) {
                continue
            }

            var pageComponent = Qt.createComponent("../" + pageInfo.path)

            if (pageComponent.status === Component.Error) {
                console.error("Error loading page:", pageInfo.path, pageComponent.errorString())
                continue
            }

            var properties = {
                navigationSection: root.navigationSection,
                navigationOrderStart: (i + 1) * 100
            }

            if (root.currentPageId === pageInfo.id) {
                var params = root.params
                for (var key in params) {
                    var value = params[key]
                    properties[key] = value
                }
            }

            var obj = pageComponent.createObject(stack, properties)

            if (!Boolean(obj)) {
                continue
            }

            obj.hideRequested.connect(function () {
                root.hide()
            })

            root.prv.pagesObjects[pageInfo.id] = obj
        }
    }

    PreferencesModel {
        id: preferencesModel

        onCurrentPageIdChanged: function (currentPageId) {
            prv.updateStackCurrentIndex()
        }
    }

    ColumnLayout {
        anchors.fill: parent

        spacing: 0

        RowLayout {
            Layout.fillWidth: true
            Layout.fillHeight: true

            spacing: 0

            ColumnLayout {
                Layout.fillHeight: true
                Layout.minimumWidth: 220
                Layout.preferredWidth: 220
                Layout.maximumWidth: 220
                spacing: 0

                NavigationPanel {
                    id: searchNavigation
                    name: "PreferencesSearch"
                    section: root.navigationSection
                    order: 0
                    direction: NavigationPanel.Vertical
                }

                TextInputField {
                    id: search
                    Layout.fillWidth: true
                    Layout.margins: 12
                    hint: qsTrc("preferences", "Search settings...")
                    accessible.name: hint
                    navigation.panel: searchNavigation
                    navigation.order: 0
                    navigation.onNavigationEvent: function (event) {
                        if (event.type === NavigationEvent.Down && results.count > 0) {
                            event.accepted = true
                            moveToResults()
                        }
                    }
                    clearTextButtonVisible: hasText
                    onTextChanged: function (text) { root.updateSearch(text) }
                    onEscaped: function () { if (hasText) clear() }
                    onAccepted: Qt.callLater(moveToResults)
                    Keys.onDownPressed: moveToResults()

                    function moveToResults() {
                        if (results.count > 0) {
                            results.currentIndex = 0
                            results.forceActiveFocus()
                            if (results.currentItem) {
                                results.currentItem.navigation.requestActive()
                            }
                        }
                    }
                }

                PreferencesMenu {
                    Layout.fillWidth: true
                    Layout.fillHeight: true
                    visible: !root.searchQuery
                    navigation.section: root.navigationSection
                    navigation.order: 1
                    model: preferencesModel
                }

                ListView {
                    id: results
                    Layout.fillWidth: true
                    Layout.fillHeight: true
                    visible: root.searchMatches.length > 0
                    clip: true
                    model: root.searchMatches
                    keyNavigationEnabled: true
                    Keys.onReturnPressed: root.openSearchResult(currentIndex)
                    Keys.onEnterPressed: root.openSearchResult(currentIndex)
                    Keys.onEscapePressed: {
                        search.clear()
                        search.forceActiveFocus()
                    }

                    delegate: FlatButton {
                        width: ListView.view.width
                        text: modelData.title
                        navigation.panel: searchNavigation
                        navigation.order: index + 2
                        accentButton: index === results.currentIndex
                        navigation.onActiveChanged: {
                            if (navigation.active) {
                                results.currentIndex = index
                            }
                        }
                        onClicked: {
                            results.currentIndex = index
                            results.forceActiveFocus()
                            navigation.requestActive()
                            root.openSearchResult(index)
                        }
                    }
                }

                StyledTextLabel {
                    Layout.fillWidth: true
                    Layout.margins: 12
                    visible: root.searchQuery.length > 0 && root.searchMatches.length === 0
                    text: qsTrc("preferences", "No matching settings")
                }

                Item {
                    Layout.fillHeight: true
                    visible: root.searchQuery.length > 0 && root.searchMatches.length === 0
                }
            }

            SeparatorLine {
                orientation: Qt.Vertical
            }

            StackLayout {
                id: stack
                Layout.fillWidth: true
                Layout.fillHeight: true
            }
        }

        SeparatorLine {}

        PreferencesButtonsPanel {
            id: buttonsPanel

            Layout.fillWidth: true
            Layout.preferredHeight: 70

            navigation.section: root.navigationSection
            navigation.order: 100000

            onRevertFactorySettingsRequested: {
                var pages = preferencesModel.availablePages()

                for (var i in pages) {
                    var page = pages[i]
                    var obj = root.prv.pagesObjects[page.id]
                    if (Boolean(obj)) {
                        obj.reset()
                    }
                }

                preferencesModel.resetFactorySettings()
            }

            onRejectRequested: {
                preferencesModel.cancel()
                root.reject()
            }

            onApplyRequested: {
                var ok = true
                var pages = preferencesModel.availablePages()

                for (var i in pages) {
                    var page = pages[i]
                    var obj = root.prv.pagesObjects[page.id]
                    if (Boolean(obj)) {
                        ok &= obj.apply()
                    }
                }

                if (ok) {
                    preferencesModel.apply()
                    root.hide()
                }
            }
        }
    }
}
