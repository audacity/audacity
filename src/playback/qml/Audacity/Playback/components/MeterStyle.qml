/*
* Audacity: A Digital Audio Editor
*/
pragma Singleton

import QtQuick 2.15

import Muse.Ui 1.0
import Muse.UiComponents

QtObject {
    readonly property color clippedColor: ui.theme.extra["meter_clipped_color"]
    readonly property color noClippedColor: ui.theme.buttonColor
    readonly property color rmsColor: ui.theme.accentColor
    readonly property color rmsOverlayColor: ui.theme.extra["meter_rms_overlay_color"]
    readonly property color defaultColor: ui.theme.accentColor
    readonly property color gradientColorGreen: ui.theme.extra["meter_gradient_green_color"]
    readonly property color gradientColorYellow: ui.theme.extra["meter_gradient_yellow_color"]
    readonly property color gradientColorRed: ui.theme.extra["meter_clipped_color"]
    readonly property color meterBackgroundColor: Utils.colorWithAlpha(ui.theme.strokeColor, 0.7)
    readonly property color maxPeakMarkerColor: ui.theme.extra["meter_max_peak_marker_color"]
}
