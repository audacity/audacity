#pragma once

#include "../iwavepainter.h"

#include "project/iaudacityproject.h"

#include "au3wrap/au3types.h"
#include "WaveMetrics.h"
#include "sampledata.h"

namespace au::projectscene::samplespainterutils {
float FromDB(float value, double dBRange);
float ValueOfPixel(int yy, int height, bool offset, bool dB, double dBRange, float zoomMin, float zoomMax);
int getWaveYPos(float value, float min, float max, int height, bool dB, bool outer, float dBr, bool clip);
void drawBackground(QPainter& painter, const au::projectscene::WaveMetrics& metrics, const IWavePainter::Style& style,
                    const double trimLeft);
void drawBaseLine(QPainter& painter, const au::projectscene::WaveMetrics& metrics, const IWavePainter::Style& style);
SampleData getSampleData(const au::au3::Au3WaveClip& clip, int channelIndex, const au::projectscene::WaveMetrics& metrics, bool dB,
                         float dBRange, float zoomMax, float zoomMin);
std::optional<int> isNearSample(std::shared_ptr<au::project::IAudacityProject> project, const trackedit::ClipKey& clipKey,
                                const QPoint& position, const IWavePainter::Params& params);
void setLastClickPos(const unsigned int currentChannel, std::shared_ptr<au::project::IAudacityProject> project,
                     const trackedit::ClipKey& clipKey, const QPoint& lastPosition, const QPoint& currentPosition,
                     const IWavePainter::Params& params);
}
