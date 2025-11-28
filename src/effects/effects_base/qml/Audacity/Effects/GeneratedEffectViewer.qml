/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

import Audacity.Effects

Rectangle {
    id: root

    property int instanceId: -1

    implicitWidth: 400
    implicitHeight: 300

    color: ui.theme.backgroundPrimaryColor

    ColumnLayout {
        anchors.fill: parent
        anchors.margins: 16
        spacing: 16

        StyledTextLabel {
            Layout.fillWidth: true
            text: qsTrc("effects", "Generated UI")
            font: ui.theme.headerBoldFont
            horizontalAlignment: Text.AlignHCenter
        }

        Rectangle {
            Layout.fillWidth: true
            Layout.fillHeight: true
            color: ui.theme.backgroundSecondaryColor
            border.color: ui.theme.strokeColor
            border.width: 1
            radius: 4

            ColumnLayout {
                anchors.fill: parent
                anchors.margins: 16
                spacing: 12

                StyledTextLabel {
                    Layout.fillWidth: true
                    text: qsTrc("effects", "Auto-generated UI based on plugin parameters")
                    horizontalAlignment: Text.AlignHCenter
                }

                StyledTextLabel {
                    Layout.fillWidth: true
                    text: qsTrc("effects", "Instance ID: %1").arg(root.instanceId)
                    horizontalAlignment: Text.AlignHCenter
                }

                Item {
                    Layout.fillHeight: true
                }

                StyledTextLabel {
                    Layout.fillWidth: true
                    text: qsTrc("effects", "Parameter extraction and UI generation coming soon...")
                    horizontalAlignment: Text.AlignHCenter
                    opacity: 0.6
                }
            }
        }
    }

    // Dummy methods to match the interface expected by DestructiveEffectsViewerDialog
    function preview() {
        console.log("GeneratedEffectViewer: preview() called")
    }

    property bool isApplyAllowed: true
    property bool usesPresets: true
}
