#pragma once

#include "isamplespainter.h"

namespace au::projectscene {
struct SampleData {
    std::vector<int> y {};
    std::vector<int> x {};

    SampleData() = default;

    SampleData(std::vector<int> pY, std::vector<int> pX)
    : y(std::move(pY)), x(std::move(pX)) {}

    size_t size() const { return x.size(); }
};

class SamplesPainter : public ISamplesPainter
{
public:
    SamplesPainter() = default;
    void paint(int channelIndex, QPainter& painter, const WaveMetrics& metrics, const Style& style, const au::au3::Au3WaveTrack& track, const au::au3::Au3WaveClip& clip) override;
private:
    void drawBackground(QPainter& painter, const WaveMetrics& metrics, const Style& style, const double trimLeft);
    void drawBaseLine(QPainter& painter, const WaveMetrics& metrics, const Style& style);
    int getWaveYPos(float value, float min, float max,
                int height, bool dB, bool outer,
                float dBr, bool clip);
    void drawSampleHead(const SampleData& samples, const au::projectscene::WaveMetrics& metrics, QPainter& painter, const Style& style);
    void drawSampleStalk(const SampleData& samples, int yZero, const au::projectscene::WaveMetrics& metrics, QPainter& painter, const Style& style);
    void drawConnectingPoints(const SampleData& samples, const au::projectscene::WaveMetrics& metrics, QPainter& painter);
    SampleData getSampleData(const au::au3::Au3WaveClip& clip, int channelIndex, const WaveMetrics& metrics, bool dB, float dBRange, float zoomMax, float zoomMin);
    bool showDraggablePoints(const au::au3::Au3WaveClip& clip, double zoom);
};
}