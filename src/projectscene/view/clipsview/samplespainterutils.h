#pragma once

#include "iwavepainter.h"

#include "au3wrap/au3types.h"
#include "au3/WaveMetrics.h"
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
std::optional<int> isNearSample(const au::au3::Au3WaveTrack& waveTrack, const au::au3::Au3WaveClip& waveClip, const QPoint& position,
                                const WaveMetrics& wm);
void setLastClickPos(const unsigned int currentChannel, const au::au3::Au3WaveTrack& waveTrack, au::au3::Au3WaveClip& waveClip,
                     const std::optional<QPoint>& lastPosition, const QPoint& position, const WaveMetrics& wm, bool enableMultiSampleEdit);
}
