/*
 * Audacity: A Digital Audio Editor
 */
pragma Singleton

import QtQml

import Audacity.ProjectScene

QtObject {
    readonly property var all: [
        {
            value: FadeType.S_CURVE,
            text: qsTrc("projectscene", "S-curve")
        },
        {
            value: FadeType.LINEAR,
            text: qsTrc("projectscene", "Linear")
        },
        {
            value: FadeType.EXPONENTIAL,
            text: qsTrc("projectscene", "Exponential")
        },
        {
            value: FadeType.LOGARITHMIC,
            text: qsTrc("projectscene", "Logarithmic")
        },
        {
            value: FadeType.SQUARE_ROOT,
            text: qsTrc("projectscene", "Square root")
        },
        {
            value: FadeType.EQUAL_POWER,
            text: qsTrc("projectscene", "Equal power")
        }
    ]
}
