/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15

import Muse.Ui 1.0
import Muse.UiComponents

import Audacity.Playback 1.0

HorizontalVolumePressureMeterItem {
    id: root

    width: parent.width
    height: 6

    opacity: enabled ? 1.0 : ui.theme.itemOpacityDisabled

    clippedColor: ui.theme.extra["meter_clipped_color"]
    noClippedColor: ui.theme.buttonColor
    rmsColor: ui.theme.accentColor
    rmsOverlayColor: ui.theme.extra["meter_rms_overlay_color"]
    defaultColor: ui.theme.accentColor
    gradientColorGreen: ui.theme.extra["meter_gradient_green_color"]
    gradientColorYellow: ui.theme.extra["meter_gradient_yellow_color"]
    gradientColorRed: ui.theme.extra["meter_clipped_color"]
    meterBackgroundColor: Utils.colorWithAlpha(ui.theme.strokeColor, 0.7)
    maxPeakMarkerColor: ui.theme.extra["meter_max_peak_marker_color"]
}
