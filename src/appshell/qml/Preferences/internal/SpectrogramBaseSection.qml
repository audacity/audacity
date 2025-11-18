/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick 2.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0

BaseSection {
    id: root

    readonly property int prefsColumnWidth: 68
    readonly property int prefsColumnSpacing: 8

    readonly property int smallControlWidth: prefsColumnWidth
    readonly property int mediumControlWidth: 2 * prefsColumnWidth + prefsColumnSpacing
    readonly property int largeControlWidth: 3 * prefsColumnWidth + 2 * prefsColumnSpacing

    readonly property int narrowSpacing: 8
    readonly property int mediumSpacing: 16

    signal activeFocusRequested(var rect)

    onFocusChanged: {
        if (activeFocus) {
            root.activeFocusRequested(Qt.rect(x, y, width, height))
        }
    }
}
