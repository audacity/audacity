/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15

import Muse.Ui 1.0

import Audacity.Playback 1.0
import Audacity.ProjectScene 1.0

VolumePressureMeterItem {
    id: root

    width: indicatorWidth

    opacity: enabled ? 1.0 : ui.theme.itemOpacityDisabled

    clippedColor: MeterStyle.clippedColor
    noClippedColor: MeterStyle.noClippedColor
    rmsColor: MeterStyle.rmsColor
    rmsOverlayColor: MeterStyle.rmsOverlayColor
    defaultColor: MeterStyle.defaultColor
    gradientColorGreen: MeterStyle.gradientColorGreen
    gradientColorYellow: MeterStyle.gradientColorYellow
    gradientColorRed: MeterStyle.gradientColorRed
    meterBackgroundColor: MeterStyle.meterBackgroundColor
    maxPeakMarkerColor: MeterStyle.maxPeakMarkerColor
}
