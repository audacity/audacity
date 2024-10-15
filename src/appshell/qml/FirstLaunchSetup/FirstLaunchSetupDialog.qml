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
import Muse.UiComponents 1.0
import Audacity.AppShell 1.0

StyledDialogView {
    id: root

    title: qsTrc("appshell/gettingstarted", "Getting started")

    contentWidth: 576
    contentHeight: 384

    margins: 20

    readonly property Page currentPage: pageLoader.item as Page

    FirstLaunchSetupModel {
        id: model
    }

    Component.onCompleted: {
        model.load()
    }

    onAboutToClose: function(closeEvent) {
        if (model.canFinish) {
            model.finish()
            return
        }

        let shouldClose = model.askAboutClosingEarly()
        if (!shouldClose) {
            closeEvent.accepted = false
            return
        }

        model.finish()
    }

    ColumnLayout {
        id: content

        anchors.fill: parent
        anchors.leftMargin: 28
        anchors.rightMargin: 28
        spacing: 24

        PageIndicator {
            Layout.alignment: Qt.AlignCenter
            count: model.numberOfPages
            currentIndex: model.currentPageIndex
        }

        Loader {
            id: pageLoader

            Layout.fillWidth: true
            Layout.fillHeight: true
            Layout.topMargin: -8
            source: model.currentPage.url

            onLoaded: {
                item.navigationSection = root.navigationSection
                item.activeButtonTitle = buttons.activeButton.text

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

        RowLayout {
            id: buttons

            spacing: 12

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

                text: qsTrc("global", "Back")
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
                    if (Boolean(buttons.lastPressedButton)) {
                        buttons.lastPressedButton.navigation.accessible.ignored = true
                    }

                    buttons.lastPressedButton = backButton
                    pageLoader.item.resetFocus()
                    model.currentPageIndex--
                }
            }

            Item {
                Layout.fillWidth: true // spacer
            }

            FlatButton {
                id: extraButton

                Layout.alignment: Qt.AlignRight

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
                }
            }

            FlatButton {
                id: nextStepButton

                Layout.alignment: Qt.AlignRight

                text: model.canFinish ? qsTrc("appshell/gettingstarted", "Finish")
                                      : qsTrc("global", "Next")
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
                    if (model.canFinish) {
                        model.finish()
                        root.hide()
                        return
                    }

                    if (Boolean(buttons.lastPressedButton)) {
                        buttons.lastPressedButton.navigation.accessible.ignored = true
                    }

                    buttons.lastPressedButton = nextStepButton
                    pageLoader.item.resetFocus()
                    model.currentPageIndex++
                }
            }
        }
    }
}
