import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

Rectangle {
    id: root

    enum Type {
        Info,
        Warning,
        Error
    }

    property int type: Callout.Type.Info
    property string title: ""
    property string text: ""

    readonly property string kind: {
        switch (root.type) {
        case Callout.Type.Warning:
            return "warning"
        case Callout.Type.Error:
            return "error"
        }
        return "info"
    }

    implicitHeight: row.implicitHeight + 2 * prv.padding

    radius: 4
    color: ui.theme.extra["callout_" + root.kind + "_background_color"]
    border.color: ui.theme.extra["callout_" + root.kind + "_border_color"]
    border.width: 1

    QtObject {
        id: prv

        readonly property int padding: 12
    }

    RowLayout {
        id: row

        anchors.fill: parent
        anchors.margins: prv.padding
        spacing: 12

        StyledIconLabel {
            Layout.alignment: Qt.AlignTop
            iconCode: root.type === Callout.Type.Warning ? IconCode.WARNING : root.type === Callout.Type.Error ? IconCode.ERROR : IconCode.INFO
            font.pixelSize: 16
            color: ui.theme.extra["callout_" + root.kind + "_icon_color"]
        }

        ColumnLayout {
            Layout.fillWidth: true
            spacing: 4

            StyledTextLabel {
                Layout.fillWidth: true
                visible: root.title !== ""
                text: root.title
                font: ui.theme.bodyBoldFont
                horizontalAlignment: Text.AlignLeft
                wrapMode: Text.Wrap
            }

            StyledTextLabel {
                Layout.fillWidth: true
                visible: root.text !== ""
                text: root.text
                horizontalAlignment: Text.AlignLeft
                wrapMode: Text.Wrap
            }
        }
    }
}
