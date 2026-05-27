import QtQuick
import QtQuick.Layouts
import QtQuick.Controls

import Muse.Ui
import Muse.UiComponents
import Muse.GraphicalEffects

Rectangle {
    id: root

    property string iconUrl: ""
    property string title: ""
    property string subtitle: ""
    property string effectCode: ""

    signal getEffectClicked(string code)

    QtObject {
        id: prv

        readonly property int spaceS: 4
        readonly property int spaceM: 8
        readonly property int spaceL: 12
        readonly property int spaceXL: 16
        readonly property int spaceXXL: 24

        readonly property int borderWidth: 1
        readonly property int borderRadius: 4

        readonly property int cardWidth: 304
        readonly property int cardHeight: 120
        readonly property int iconSize: 96
        readonly property int buzySize: 32
        readonly property int iconPlaceholderSize: 48
        readonly property int getItButtonWidth: 172
        readonly property int getItButtonHeight: 24
    }

    width: prv.cardWidth
    height: prv.cardHeight

    radius: prv.borderRadius
    color: ui.theme.backgroundPrimaryColor
    border.width: prv.borderWidth
    border.color: ui.theme.strokeColor

    RowLayout {
        anchors.fill: parent
        anchors.margins: prv.spaceL
        spacing: prv.spaceL

        Rectangle {
            id: previewRect

            Layout.preferredWidth: prv.iconSize
            Layout.preferredHeight: prv.iconSize
            radius: prv.borderRadius
            color: effectImage.status === Image.Ready ? "transparent" : ui.theme.backgroundSecondaryColor
            border.width: effectImage.status === Image.Ready ? 0 : prv.borderWidth
            border.color: ui.theme.strokeColor
            clip: true

            Image {
                id: effectImage
                anchors.fill: parent
                source: root.iconUrl
                fillMode: Image.PreserveAspectCrop
                asynchronous: true
                cache: true
                visible: status === Image.Ready

                layer.enabled: ui.isEffectsAllowed
                layer.effect: RoundedCornersEffect {
                    radius: previewRect.radius
                }
            }

            StyledIconLabel {
                anchors.centerIn: parent
                iconCode: IconCode.PLUGIN
                font.pixelSize: prv.iconPlaceholderSize
                color: ui.theme.fontPrimaryColor
                visible: effectImage.status !== Image.Ready && effectImage.status !== Image.Loading
            }

            StyledBusyIndicator {
                anchors.centerIn: parent
                width: prv.buzySize
                height: prv.buzySize
                visible: effectImage.status === Image.Loading
                running: visible
            }
        }

        ColumnLayout {
            Layout.fillWidth: true
            Layout.fillHeight: true
            spacing: prv.spaceS

            StyledTextLabel {
                Layout.fillWidth: true
                text: root.title
                font: ui.theme.largeBodyBoldFont
                horizontalAlignment: Text.AlignLeft
                maximumLineCount: 2
                elide: Text.ElideRight
                wrapMode: Text.Wrap
            }

            StyledTextLabel {
                Layout.fillWidth: true
                text: root.subtitle
                font: ui.theme.bodyFont
                horizontalAlignment: Text.AlignLeft
                wrapMode: Text.Wrap
                maximumLineCount: 2
                elide: Text.ElideRight
            }

            Item {
                Layout.fillHeight: true
            }

            FlatButton {
                Layout.preferredHeight: prv.getItButtonHeight
                Layout.preferredWidth: prv.getItButtonWidth
                text: qsTrc("projectscene", "Get it on MuseHub")
                accentButton: true
                onClicked: root.getEffectClicked(root.effectCode)
            }
        }
    }
}
