/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick 2.15
import QtQuick.Window 2.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0
import Audacity.AppShell 1.0

Page {
    title: qsTrc("appshell/gettingstarted", "Pre-alpha build")
    explanation: qsTrc("appshell/gettingstarted", "Many features are currently unavailable, but our team is actively developing and implementing them for future releases.")

    titleContentSpacing: 12

    Image {
        id: image
        anchors.fill: parent
        fillMode: Image.PreserveAspectCrop
        source: "resources/UnderConstruction.png"
        sourceSize: Qt.size(width * Screen.devicePixelRatio, height * Screen.devicePixelRatio)
    }
}
