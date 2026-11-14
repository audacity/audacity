/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15
import QtQuick.Layouts 1.15

import Muse.Ui 1.0
import Muse.UiComponents

import Audacity.AppShell

ColumnLayout {
    id: root

    required property AboutModel model

    spacing: prv.contentSpacing

    QtObject {
        id: prv

        readonly property int versionTextSpacing: 12
        readonly property int contentSpacing: 16
        readonly property int contentMargin: 16
        readonly property int contentTextMargin: 12
        readonly property int contentTextSpacing: 8

        readonly property string versionSubtitle: qsTrc("appshell/about", "Audacity the free, open source, cross-platform software for recording and editing sounds.")
    }

    Image {
        Layout.fillWidth: true

        source: "qrc:/resources/AboutBanner.png"
        sourceSize.width: root.width

        MouseArea {
            anchors.fill: parent

            property int clickCount: 0

            onClicked: {
                clickCount++
                if (clickCount % 3 === 0) {
                    root.model.toggleDevMode()
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

            text: root.model.appVersion()
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
                    model: root.model.creditList()

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
                            wrapMode: Text.WordWrap
                            textFormat: Text.RichText
                            font: ui.theme.bodyFont

                            text: modelData.credits.map(function (c) {
                                let isRaw = c.raw && c.raw.length > 0
                                if (isRaw) {
                                    return c.raw
                                }

                                let isUrl = c.url && c.url.length > 0
                                if (isUrl) {
                                    return '<a href="' + c.url + '">' + c.name + '</a>'
                                }

                                return c.role ? c.name + ", " + c.role : c.name
                            }).join("<br>")
                        }
                    }
                }

                StyledTextLabel {
                    text: {
                        let websiteUrl = root.model.appUrl()
                        return qsTrc("appshell/about", "Audacity website: %1").arg('<a href="' + websiteUrl.url + '">' + websiteUrl.displayName + '</a>')
                    }
                    font: ui.theme.bodyFont
                }

                Column {
                    width: parent.width
                    spacing: 0

                    StyledTextLabel {
                        text: qsTrc("appshell/about", "<b>Audacity®</b> software is copyright © 1999-%1 Audacity Team.").arg(new Date().getFullYear())
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
