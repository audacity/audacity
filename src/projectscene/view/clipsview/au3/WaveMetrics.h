/*
* Audacity: A Digital Audio Editor
*/
#pragma once

namespace au::projectscene {
struct WaveMetrics
{
    double top = 0.0;
    double left = 0.0;
    double height = 0.0;
    double width = 0.0;

    double zoom = 1.0;

    double fromTime = 0.0;
    double toTime = 0.0;

    double selectionStartTime = 0.0;
    double selectionEndTime = 0.0;
};
}
