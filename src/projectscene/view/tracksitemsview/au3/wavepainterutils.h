#pragma once

#include "project/iaudacityproject.h"

#include "../iwavepainter.h"
#include "WaveMetrics.h"

namespace au::projectscene::wavepainterutils {
IWavePainter::PlotType getPlotType(std::shared_ptr<au::project::IAudacityProject> project, const trackedit::ClipKey& clipKey, double zoom);
WaveMetrics getWaveMetrics(std::shared_ptr<au::project::IAudacityProject> project, const trackedit::ClipKey& clipKey,
                           const IWavePainter::Params& params, bool snapToSamples = false);

//! The same from what is known about the clip, without looking the clip up
IWavePainter::PlotType getPlotType(double samplesPerSecond, double zoom);
WaveMetrics getWaveMetrics(double playStartTime, double trimLeft, double samplesPerSecond, const IWavePainter::Params& params,
                           bool snapToSamples = false);
}
