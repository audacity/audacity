/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QPainter>

#include "au3wavepainter.h"
#include "au3wrap/au3types.h"
#include "WaveMetrics.h"

using namespace au::au3;

using Style = au::projectscene::Au3WavePainter::Style;

namespace au::projectscene {

void DrawIndividualSamples(int channelIndex, QPainter& painter,
                           const Style& style,
                           const WaveMetrics& metrics,
                           const Au3WaveClip& clip,
                           float zoomMin, float zoomMax,
                           bool dB, float dBRange,
                           bool showPoints);

}