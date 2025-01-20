/*
 * Audacity: A Digital Audio Editor
 */
 import QtQuick 2.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0


/*
 * Copied from StyledDialogView, just removing the NavigationSection, which, when calling `requestActive()`, breaks the shortcut context.
 * This is a temporary solution. We will soon need realtime effect dialogs to be navigable with the keyboard, at which point a proper
 * solution will be needed.
 */
DialogView {
    id: root

    default property alias contentData: contentBody.data

    property alias background: contentBackground

    property alias width: rootContainer.width
    property alias height: rootContainer.height

    property int margins: 0

    property bool closeOnEscape : true

    contentWidth: 240
    contentHeight: contentBody.childrenRect.height

    onOpened: {
        accessibilityActiveTimer.start()
    }

    signal accessibilityActivateRequested()

    property Timer accessibilityActiveTimer: Timer {
        interval: 500
        repeat: false

        onTriggered: {
            root.accessibilityActivateRequested()
        }
    }

    contentItem: FocusScope {
        id: rootContainer
        width: contentBody.width + root.margins * 2
        height: contentBody.height + root.margins * 2

        implicitWidth: contentBody.implicitWidth + root.margins * 2
        implicitHeight: contentBody.implicitHeight + root.margins * 2

        ItemWithDropShadow {
            anchors.fill: parent
            shadow.visible: root.frameless

            Rectangle {
                id: contentBackground
                anchors.fill: parent
                color: ui.theme.backgroundPrimaryColor
                radius: root.frameless ? 4 : 0
                border.width: root.frameless ? 1 : 0
                border.color: ui.theme.strokeColor
            }
        }

        Item {
            id: contentBody
            anchors.fill: parent
            anchors.margins: root.margins

            implicitWidth: root.contentWidth
            implicitHeight: root.contentHeight
        }
    }
}
