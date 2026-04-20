/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 *
 * MuseScore
 * Music Composition & Notation
 *
 * Copyright (C) 2021-2024 MuseScore BVBA and others
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

    title: qsTrc("appshell/about", "About Audacity")

    contentHeight: 600
    contentWidth: 720

    AboutModel {
        id: aboutModel
    }

    QtObject {
        id: prv

        readonly property int tabTopMargin: 11
        readonly property int tabBottomMargin: 11
        readonly property int tabButtonSpacing: 32

        readonly property int versionTextSpacing: 12

        readonly property int contentMargin: 16
        readonly property int contentSpacing: 16
        readonly property int contentTextMargin: 12
        readonly property int contentTextSpacing: 8
    }

    ColumnLayout {
        anchors.fill: parent
        spacing: 0

        StyledTabBar {
            id: tabBar

            Layout.topMargin: prv.tabTopMargin
            Layout.bottomMargin: prv.tabBottomMargin
            Layout.alignment: Qt.AlignHCenter
            spacing: prv.tabButtonSpacing

            StyledTabButton {
                text: qsTrc("appshell/about", "Audacity")
            }

            StyledTabButton {
                text: qsTrc("appshell/about", "Legal")
            }
        }

        StackLayout {
            id: stackLayout

            Layout.fillWidth: true
            Layout.fillHeight: true

            currentIndex: tabBar.currentIndex

            StyledFlickable {
                id: audacityFlickable
                contentHeight: audacityContent.height

                Column {
                    id: audacityContent
                    width: audacityFlickable.width

                    spacing: prv.contentSpacing

                    Image {
                        width: parent.width
                        source: "qrc:/resources/AboutBanner.png"
                        fillMode: Image.PreserveAspectFit

                        MouseArea {
                            anchors.fill: parent

                            property int clickCount: 0

                            onClicked: {
                                clickCount++
                                if (clickCount % 3 === 0) {
                                    aboutModel.toggleDevMode()
                                }
                            }
                        }
                    }

                    Column {
                        anchors.left: parent.left
                        anchors.right: parent.right
                        anchors.leftMargin: prv.contentMargin
                        anchors.rightMargin: prv.contentMargin

                        spacing: prv.versionTextSpacing

                        StyledTextLabel {
                            width: parent.width

                            horizontalAlignment: Text.AlignHCenter

                            text: aboutModel.appVersion()
                            font: ui.theme.headerBoldFont
                        }

                        StyledTextLabel {
                            width: parent.width

                            horizontalAlignment: Text.AlignHCenter

                            text: qsTrc("appshell/about", "Audacity the free, open source, cross-platform software for recording and editing sounds.")
                            font: ui.theme.bodyFont
                        }
                    }

                    Rectangle {
                        anchors.left: parent.left
                        anchors.right: parent.right
                        anchors.leftMargin: prv.contentMargin
                        anchors.rightMargin: prv.contentMargin

                        height: creditsInner.height + prv.contentTextMargin * 2

                        color: ui.theme.isDark ? ui.theme.extra["black_color"] : ui.theme.extra["white_color"]

                        Column {
                            id: creditsInner

                            anchors.top: parent.top
                            anchors.left: parent.left
                            anchors.right: parent.right

                            anchors.margins: prv.contentTextMargin

                            spacing: prv.contentTextSpacing

                            StyledTextLabel {
                                width: parent.width
                                text: qsTrc("appshell/about", "Credits")
                                font: ui.theme.bodyBoldFont
                                horizontalAlignment: Text.AlignLeft
                            }

                            Repeater {
                                model: aboutModel.creditList()

                                Column {
                                    width: parent.width
                                    spacing: prv.contentTextSpacing

                                    Loader {
                                        width: parent.width

                                        readonly property bool hasSubtitle: modelData.subtitle && modelData.subtitle.length > 0

                                        sourceComponent: hasSubtitle ? titleWithSubtitle : titleOnly

                                        Component {
                                            id: titleOnly

                                            StyledTextLabel {
                                                width: parent.width

                                                horizontalAlignment: Text.AlignLeft

                                                text: modelData.title
                                                font: ui.theme.bodyBoldFont
                                            }
                                        }

                                        Component {
                                            id: titleWithSubtitle

                                            Column {
                                                width: parent.width
                                                spacing: prv.contentTextSpacing

                                                StyledTextLabel {
                                                    width: parent.width

                                                    horizontalAlignment: Text.AlignLeft

                                                    text: modelData.title
                                                    font: ui.theme.bodyBoldFont
                                                }

                                                StyledTextLabel {
                                                    width: parent.width

                                                    horizontalAlignment: Text.AlignLeft

                                                    text: modelData.subtitle
                                                    font: ui.theme.bodyFont
                                                }
                                            }
                                        }
                                    }

                                    StyledTextLabel {
                                        width: parent.width

                                        horizontalAlignment: Text.AlignLeft

                                        text: modelData.credits.map(function (c) {
                                            let isUrl = c.url && c.url.length > 0

                                            if (isUrl) {
                                                return qsTrc("appshell/about", "%1").arg('<a href="' + c.url + '">' + c.name + '</a>')
                                            }

                                            return c.role ? c.name + ", " + c.role : c.name
                                        }).join("<br>")
                                    }
                                }
                            }

                            StyledTextLabel {
                                text: {
                                    let websiteUrl = aboutModel.appUrl()

                                    qsTrc("appshell/about", "Audacity website: %1").arg('<a href="' + websiteUrl.url + '">' + websiteUrl.displayName + '</a>')
                                }
                                font: ui.theme.bodyFont
                            }

                            Column {
                                width: parent.width
                                spacing: 0

                                StyledTextLabel {
                                    text: qsTrc("appshell/about", "<b>Audacity®</b> software is copyright © 1999-2024 Audacity Team.")
                                    font: ui.theme.bodyFont
                                }

                                StyledTextLabel {
                                    text: qsTrc("appshell/about", "The name <b>Audacity</b> is a registered trademark.")
                                    font: ui.theme.bodyFont
                                }
                            }
                        }
                    }
                }
            }

            StyledFlickable {
                id: legalFlickable
                contentHeight: legalContent.height

                Column {
                    id: legalContent
                    width: legalFlickable.width

                    Column {
                        anchors.left: parent.left
                        anchors.right: parent.right
                        anchors.leftMargin: prv.contentMargin
                        anchors.rightMargin: prv.contentMargin

                        spacing: prv.versionTextSpacing

                        StyledTextLabel {
                            width: parent.width

                            horizontalAlignment: Text.AlignHCenter

                            text: qsTrc("appshell/about", "Privacy Policy")
                            font: ui.theme.headerBoldFont
                        }

                        StyledTextLabel {
                            width: parent.width

                            horizontalAlignment: Text.AlignHCenter

                            text: qsTrc("appshell/about", "App update checking and error reporting require network access. These features are optional.\nSee our Privacy policy for more info.")
                            font: ui.theme.bodyFont
                        }
                    }
                }
            }
        }

        RowLayout {
            Layout.alignment: Qt.AlignRight
            Layout.rightMargin: 16
            Layout.bottomMargin: 16

            FlatButton {
                text: qsTrc("global", "OK")

                onClicked: {
                    root.hide()
                }
            }
        }
    }
}
