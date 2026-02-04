/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 *
 * MuseScore
 * Music Composition & Notation
 *
 * Copyright (C) 2021 MuseScore BVBA and others
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
import Audacity.AppShell

StyledDialogView {
    id: root

    objectName: "FirstLaunchSetupDialog"
    title: model.dialogTitle

    contentWidth: 560
    contentHeight: 442

    modal: true
    frameless: true
    closeOnEscape: false

    margins: 0

    readonly property Page currentPage: pageLoader.item as Page

    function endSetup() {
        model.finish()
        root.hide()
    }

    function advanceToNextPage() {
        if (root.currentPage) {
            root.currentPage.nextButtonClicked()
        }

        if (model.canFinish) {
            endSetup()
            return
        }

        if (Boolean(buttons.lastPressedButton)) {
            buttons.lastPressedButton.navigation.accessible.ignored = true
        }

        pageLoader.item.resetFocus()
        model.currentPageIndex++
    }

    FirstLaunchSetupModel {
        id: model
    }

    Component.onCompleted: {
        model.load()
    }

    ColumnLayout {
        id: content

        anchors.fill: parent
        spacing: 0
        Layout.margins: 0

        StyledTextLabel {
            Layout.preferredHeight: 27 // 28 - 1 for the SeparatorLine
            Layout.leftMargin: 8
            Layout.alignment: Qt.AlignLeft
            text: title
            font: ui.theme.bodyFont
        }

        SeparatorLine {
            Layout.fillWidth: true
            Layout.margins: 0
            Layout.preferredHeight: 1
        }

        Loader {
            id: pageLoader

            Layout.fillWidth: true
            Layout.preferredHeight: 366
            source: model.currentPage.url

            onLoaded: {
                item.navigationSection = root.navigationSection
                item.activeButtonTitle = buttons.activeButton.text

                if (item.navNextPageRequested) {
                    item.navNextPageRequested.connect(function() {
                        advanceToNextPage()
                    })
                }

                navigationActiveTimer.start()
            }

            Timer {
                id: navigationActiveTimer

                interval: 1000
                repeat: false

                onTriggered: {
                    buttons.activeButton.navigation.accessible.ignored = true
                    buttons.activeButton.navigation.requestActive()
                    pageLoader.item.readInfo()
                }
            }
        }

        SeparatorLine {
            Layout.fillWidth: true
            Layout.margins: 0
            Layout.preferredHeight: 1
        }

        RowLayout {
            id: buttons
            Layout.preferredHeight: 47 // 48 - 1 for the SeparatorLine

            Layout.fillWidth: true
            Layout.leftMargin: 12
            Layout.rightMargin: 12
            Layout.topMargin: 9 // 10 - 1 for the SeparatorLine
            Layout.bottomMargin: 10
            spacing: 8

            StyledTextLabel {
                Layout.topMargin: 6
                Layout.bottomMargin: 6
                Layout.alignment: Qt.AlignLeft
                text: model.formatPageProgress(model.currentPageIndex + 1, model.numberOfPages)
                font: ui.theme.bodyFont
            }

            Item {
                Layout.fillWidth: true // spacer to push buttons to the right
            }

            property var lastPressedButton: null
            property var activeButton: {
                if (Boolean(lastPressedButton) && lastPressedButton.visible === true) {
                    return lastPressedButton
                } else if (nextStepButton.visible === true) {
                    return nextStepButton
                } else {
                    return backButton
                }
            }

            property NavigationPanel navigationPanel: NavigationPanel {
                name: "ButtonsPanel"
                enabled: buttons.enabled && buttons.visible
                section: root.navigationSection
                order: 1 // Lower than pages
                direction: NavigationPanel.Horizontal
            }

            FlatButton {
                id: backButton

                Layout.alignment: Qt.AlignLeft
                Layout.preferredHeight: 28

                text: model.backButtonText
                enabled: model.canGoBack
                visible: model.canGoBack

                navigation.name: "BackButton"
                navigation.panel: buttons.navigationPanel
                navigation.column: 3
                navigation.onActiveChanged: {
                    if (!navigation.active) {
                        accessible.ignored = false
                        accessible.focused = true
                        pageLoader.item.resetFocus()
                    }
                }

                onClicked: {
                    if (!enabled)
                        return
                    if (Boolean(buttons.lastPressedButton)) {
                        buttons.lastPressedButton.navigation.accessible.ignored = true
                    }

                    buttons.lastPressedButton = backButton
                    pageLoader.item.resetFocus()
                    model.currentPageIndex--
                }
            }

            FlatButton {
                id: extraButton

                Layout.alignment: Qt.AlignRight
                Layout.preferredHeight: 28

                visible: root.currentPage ? Boolean(root.currentPage.extraButtonTitle) : false
                accentButton: true

                text: root.currentPage ? root.currentPage.extraButtonTitle : ""

                navigation.name: "ExtraButton"
                navigation.panel: buttons.navigationPanel
                navigation.column: 1

                onClicked: {
                    if (root.currentPage) {
                        root.currentPage.extraButtonClicked()
                    }

                    if (model.canFinish) {
                        endSetup()
                        return
                    }
                }
            }

            FlatButton {
                id: nextStepButton

                Layout.alignment: Qt.AlignRight
                Layout.preferredHeight: 28

                text: model.nextButtonText
                accentButton: !extraButton.visible

                navigation.name: "NextButton"
                navigation.panel: buttons.navigationPanel
                navigation.column: 2
                navigation.onActiveChanged: {
                    if (!navigation.active) {
                        accessible.ignored = false
                        accessible.focused = true
                        pageLoader.item.resetFocus()
                    }
                }

                onClicked: {
                    advanceToNextPage()
                }
            }
        }
    }
}
