/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "spectrogramtypes.h"
#include "./au3spectrogramtypes.h"

#include "au3-wave-track/WaveClip.h"

#include <QImage>

namespace au::spectrogram::Au3SpectrogramClipChannelPainter {
void fillImage(QImage&, const ViewInfo&, const SelectionInfo&, const SpectrogramTrackContext&, WaveClipChannel&);
}
