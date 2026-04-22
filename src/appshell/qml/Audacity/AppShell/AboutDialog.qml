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

        readonly property int tabSpacing: 16
        readonly property int tabButtonSpacing: 32

        readonly property int versionTextSpacing: 12

        readonly property int contentMargin: 16
        readonly property int contentSpacing: 16
        readonly property int contentTextMargin: 12
        readonly property int contentTextSpacing: 8

        readonly property int btnMargins: 8

        readonly property string versionSubtitle: qsTrc("appshell/about", "Audacity the free, open source, cross-platform software for recording and editing sounds.")

        readonly property string privacyPolicyUrl: "https://www.audacityteam.org/legal/privacy-notice/"
        readonly property string privacyPolicyLink: "<a href=\"%1\">%2</a>".arg(prv.privacyPolicyUrl).arg(qsTrc("appshell/about", "privacy policy"))
        readonly property string privacyTitle: qsTrc("appshell/about", "Privacy")
        readonly property string privacySubtitle: qsTrc("appshell/about", "App update checking and error reporting require network access. These features are optional.<br>See our %1 for more info.").arg(prv.privacyPolicyLink)
    }

    ColumnLayout {
        anchors.fill: parent
        spacing: prv.tabSpacing

        StyledTabBar {
            id: tabBar

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

            ColumnLayout {
                id: audacityContent

                spacing: prv.contentSpacing

                Image {
                    Layout.fillWidth: true

                    source: "qrc:/resources/AboutBanner.png"
                    sourceSize.width: root.contentWidth

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

                ColumnLayout {
                    Layout.fillWidth: true
                    Layout.preferredHeight: implicitHeight

                    spacing: prv.versionTextSpacing

                    StyledTextLabel {
                        Layout.fillWidth: true

                        horizontalAlignment: Text.AlignHCenter

                        text: aboutModel.appVersion()
                        font: ui.theme.tabBoldFont
                    }

                    StyledTextLabel {
                        Layout.fillWidth: true

                        horizontalAlignment: Text.AlignHCenter

                        text: prv.versionSubtitle
                        font: ui.theme.bodyFont
                    }
                }

                StyledFlickable {
                    id: audacityFlickable

                    Layout.fillWidth: true
                    Layout.fillHeight: true

                    contentHeight: creditsContainer.height

                    Rectangle {
                        id: creditsContainer

                        anchors.left: parent.left
                        anchors.right: parent.right
                        anchors.leftMargin: prv.contentMargin
                        anchors.rightMargin: prv.contentMargin

                        height: creditsInner.height + prv.contentTextMargin * 2

                        color: ui.theme.backgroundSecondaryColor

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
                                font: ui.theme.largeBodyBoldFont
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

            ColumnLayout {
                id: privacyContent

                spacing: prv.contentSpacing

                Column {
                    Layout.fillWidth: true
                    Layout.preferredHeight: implicitHeight

                    spacing: prv.versionTextSpacing

                    StyledTextLabel {
                        width: parent.width

                        horizontalAlignment: Text.AlignHCenter

                        text: prv.privacyTitle
                        font: ui.theme.tabBoldFont
                    }

                    StyledTextLabel {
                        width: parent.width

                        horizontalAlignment: Text.AlignHCenter
                        textFormat: Text.RichText

                        text: prv.privacySubtitle
                        font: ui.theme.bodyFont
                    }
                }

                StyledFlickable {
                    id: legalFlickable

                    Layout.fillWidth: true
                    Layout.fillHeight: true

                    contentHeight: gplContainer.height

                    Rectangle {
                        id: gplContainer

                        anchors.left: parent.left
                        anchors.right: parent.right
                        anchors.leftMargin: prv.contentMargin
                        anchors.rightMargin: prv.contentMargin

                        height: gplInner.height + prv.contentTextMargin * 2

                        color: ui.theme.backgroundSecondaryColor

                        Column {
                            id: gplInner

                            anchors.top: parent.top
                            anchors.left: parent.left
                            anchors.right: parent.right

                            anchors.margins: prv.contentTextMargin

                            Text {
                                width: parent.width
                                horizontalAlignment: Text.AlignLeft

                                textFormat: Text.RichText
                                wrapMode: Text.WordWrap
                                text: aboutModel.gplText()

                                color: ui.theme.fontPrimaryColor
                                font: ui.theme.bodyFont
                            }
                        }
                    }
                }
            }
        }

        ColumnLayout {
            Layout.fillWidth: true
            spacing: 0

            SeparatorLine {
                Layout.fillWidth: true
            }

            FlatButton {
                Layout.alignment: Qt.AlignRight
                Layout.margins: prv.btnMargins

                text: qsTrc("global", "Close")

                onClicked: {
                    root.hide()
                }
            }
        }
    }
}
