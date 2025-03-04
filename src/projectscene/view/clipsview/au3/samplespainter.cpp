#include "samplespainter.h"

#include "au3wrap/internal/domaccessor.h"
#include "Envelope.h"
#include "sampledata.h"
#include "samplespainterutils.h"
#include "wavepainterutils.h"
#include "WaveClip.h"
#include "WaveformScale.h"
#include "WaveformSettings.h"
#include "ZoomInfo.h"

constexpr auto SAMPLE_TICK_SIZE = 4;

namespace {
// Lowpass filter coefficients 1K
//    6.8831e-03   8.9553e-03   1.4863e-02   2.4159e-02   3.6016e-02   4.9308e-02
//    6.2727e-02   7.4927e-02   8.4666e-02   9.0942e-02   9.3109e-02   9.0942e-02
//    8.4666e-02   7.4927e-02   6.2727e-02   4.9308e-02   3.6016e-02   2.4159e-02
//    1.4863e-02   8.9553e-03   6.8831e-03

// Lowpass filter coeficients 100Hz
//    7.3501e-03   9.4195e-03   1.5424e-02   2.4777e-02   3.6564e-02   4.9630e-02
//    6.2698e-02   7.4486e-02   8.3842e-02   8.9849e-02   9.1919e-02   8.9849e-02
//    8.3842e-02   7.4486e-02   6.2698e-02   4.9630e-02   3.6564e-02   2.4777e-02
//    1.5424e-02   9.4195e-03   7.3501e-03

//Kaiser window
// 0.9403
//    0.9474
//    0.9541
//    0.9604
//    0.9662
//    0.9716
//    0.9765
//    0.9809
//    0.9849
//    0.9884
//    0.9915
//    0.9941
//    0.9962
//    0.9979
//    0.9991
//    0.9998
//    1.0000
//    0.9998
//    0.9991
//    0.9979
//    0.9962
//    0.9941
//    0.9915
//    0.9884
//    0.9849
//    0.9809
//    0.9765
//    0.9716
//    0.9662
//    0.9604
//    0.9541
//    0.9474
//    0.9403

//Beta 6
//  0.014873
//    0.036072
//    0.067226
//    0.109533
//    0.163608
//    0.229341
//    0.305801
//    0.391206
//    0.482956
//    0.577742
//    0.671726
//    0.760766
//    0.840685
//    0.907559
//    0.957990
//    0.989356
//    1.000000
//    0.989356
//    0.957990
//    0.907559
//    0.840685
//    0.760766
//    0.671726
//    0.577742
//    0.482956
//    0.391206
//    0.305801
//    0.229341
//    0.163608
//    0.109533
//    0.067226
//    0.036072
//    0.014873

//Beta 8
// 2.3388e-03
//    9.5997e-03
//    2.3695e-02
//    4.7241e-02
//    8.2740e-02
//    1.3220e-01
//    1.9673e-01
//    2.7619e-01
//    3.6897e-01
//    4.7185e-01
//    5.8014e-01
//    6.8797e-01
//    7.8875e-01
//    8.7581e-01
//    9.4302e-01
//    9.8548e-01
//    1.0000e+00
//    9.8548e-01
//    9.4302e-01
//    8.7581e-01
//    7.8875e-01
//    6.8797e-01
//    5.8014e-01
//    4.7185e-01
//    3.6897e-01
//    2.7619e-01
//    1.9673e-01
//    1.3220e-01
//    8.2740e-02
//    4.7241e-02
//    2.3695e-02
//    9.5997e-03
//    2.3388e-03

const auto kaiserWindow
    = std::vector<double> { 0.014873, 0.036072, 0.067226, 0.109533, 0.163608, 0.229341, 0.305801, 0.391206,
                            0.482956, 0.577742, 0.671726, 0.760766, 0.840685, 0.907559, 0.957990, 0.989356,
                            1.000000, 0.989356, 0.957990, 0.907559, 0.840685, 0.760766, 0.671726, 0.577742,
                            0.482956, 0.391206, 0.305801, 0.229341, 0.163608, 0.109533, 0.067226, 0.036072,
                            0.014873 };

const auto lowpassCoefs
    = std::vector<double> { 7.3501e-03, 9.4195e-03, 1.5424e-02, 2.4777e-02, 3.6564e-02, 4.9630e-02, 6.2698e-02, 7.4486e-02,
                            8.3842e-02, 8.9849e-02, 9.1919e-02, 8.9849e-02, 8.3842e-02, 7.4486e-02, 6.2698e-02, 4.9630e-02,
                            3.6564e-02, 2.4777e-02, 1.5424e-02, 9.4195e-03, 7.3501e-03 };

// Function to compute the sinc value at x (normalized sinc function)
double sinc(double x)
{
    if (x == 0.0) {
        return 1.0;
    }
    return sin(M_PI * x) / (M_PI * x);
}

au::projectscene::SampleData  sinc_interpolate(const std::vector<double>& x, const std::vector<double>& tx, int delta)
{
    constexpr int K = 16;
    const double T = tx[1] - tx[0];
    const int Nx = x.size();
    const int Ny = Nx * delta;

    std::vector<double> y(Ny);
    std::vector<double> ty(Ny);

    for (int nx = 0; nx < Nx; ++nx) {
        const auto ny0 = nx * delta;
        for (auto ny = ny0; ny < ny0 + delta; ++ny) {
            ty[ny] = tx[0] + ny * T / delta;
            const auto k0 = std::max(nx - K, 0) - nx;
            const auto kEnd = std::min(nx + K, Nx) - nx;
            auto sum = 0.0;
            for (auto k = k0; k < kEnd; ++k) {
                const auto nx2 = nx + k;
                const auto _s = sinc((ty[ny] - tx[nx2]) / T);
                const auto _w = kaiserWindow[K + k];
                const auto _yi = x[nx2];
                sum += _yi * _w * _s;
            }
            y[ny] = sum;
        }
    }

    return au::projectscene::SampleData(y, ty);
}

au::projectscene::SampleData apply_filter(const au::projectscene::SampleData& samples, const std::vector<double>& coefs)
{
    std::vector<double> filtered;
    filtered.reserve(samples.y.size());

    const int halfWindow = coefs.size() / 2;

    for (size_t i = 0; i < samples.y.size(); ++i) {
        double sum = 0.0;
        for (size_t j = 0; j < coefs.size(); ++j) {
            const int idx = i + j - halfWindow;
            if (idx >= 0 && idx < static_cast<int>(samples.y.size())) {
                sum += samples.y[idx] * coefs[j];
            }
        }
        filtered.push_back(sum);
    }

    return au::projectscene::SampleData(filtered, samples.x);
}

// au::projectscene::SampleData  sinc_interpolate(const std::vector<double>& y, const std::vector<double>& x, int delta)
// {
//     int N = 16;
//     double T = x[1] - x[0];
//     int L = y.size() * delta;

//     std::vector<double> yinterpolated;
//     yinterpolated.reserve(L);
//     std::vector<double> t;
//     t.reserve(L);
//     for (int i = 0; i < L; ++i) {
//         t.push_back(x[0] + i * T / delta);
//     }

//     for (int i = 0; i < L; ++i) {
//         double sum = 0.0;
//         for (int k = -N; k <= N; ++k) {
//             int idx = i / delta - k;
//             if (idx >= 0 && idx < static_cast<int>(y.size())) {
//                 sum += y[idx] * sinc((t[i] - x[idx]) / T);
//             }
//         }
//         yinterpolated.push_back(sum);
//     }

//     return au::projectscene::SampleData(yinterpolated, t);
// }

void drawConnectingPoints(const au::projectscene::SampleData& samples, const au::projectscene::WaveMetrics& metrics, QPainter& painter,
                          const au::projectscene::IWavePainter::Style& style)
{
    painter.setPen(style.samplePen);

    const size_t slen = samples.size();
    for (size_t s = 0; s < slen - 1; s++) {
        QPointF p1(metrics.left + samples.x[s], metrics.top + samples.y[s]);
        QPointF p2(metrics.left + samples.x[s + 1], metrics.top + samples.y[s + 1]);
        painter.drawLine(p1, p2);
    }
}

void drawSampleHead(const au::projectscene::SampleData& samples, const au::projectscene::WaveMetrics& metrics, QPainter& painter,
                    const au::projectscene::IWavePainter::Style& style)
{
    size_t slen = samples.size();
    const ZoomInfo zoomInfo{ metrics.fromTime, metrics.zoom };

    const auto selectedStartPosition
        = std::max(-10000, std::min(10000, static_cast<int>(zoomInfo.TimeToPosition(metrics.selectionStartTime))));
    const auto selectedEndPosition = std::max(-10000, std::min(10000, static_cast<int>(zoomInfo.TimeToPosition(metrics.selectionEndTime))));

    painter.setBrush(style.sampleBrush);

    auto pr = QRectF(0, 0, SAMPLE_TICK_SIZE, SAMPLE_TICK_SIZE);
    for (size_t s = 0; s < slen; s++) {
        if (samples.y[s] >= 0 && samples.y[s] < metrics.height) {
            if (selectedStartPosition <= samples.x[s] && samples.x[s] <= selectedEndPosition) {
                painter.setPen(style.sampleHeadSelection);
            } else {
                painter.setPen(style.sampleHead);
            }

            pr.moveLeft(metrics.left + samples.x[s] - SAMPLE_TICK_SIZE / 2);
            pr.moveTop(metrics.top + samples.y[s] - SAMPLE_TICK_SIZE / 2);

            painter.drawEllipse(pr);
        }
    }
}

void drawSampleStalk(const au::projectscene::SampleData& samples, int yZero, const au::projectscene::WaveMetrics& metrics,
                     QPainter& painter, const au::projectscene::IWavePainter::Style& style)
{
    painter.setPen(style.sampleStalk);

    const size_t slen = samples.size();
    for (size_t s = 0; s < slen; s++) {
        QPointF p1(metrics.left + samples.x[s], metrics.top + samples.y[s]);
        QPointF p2(metrics.left + samples.x[s], yZero);
        painter.drawLine(p1, p2);
    }
}
}

namespace au::projectscene {
void SamplesPainter::paint(QPainter& painter, const trackedit::ClipKey& clipKey, const IWavePainter::Params& params)
{
    au::au3::Au3Project* au3Project = reinterpret_cast<au::au3::Au3Project*>(globalContext()->currentProject()->au3ProjectPtr());
    WaveTrack* track = au::au3::DomAccessor::findWaveTrack(*au3Project, TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(track) {
        return;
    }

    std::shared_ptr<WaveClip> waveClip = au::au3::DomAccessor::findWaveClip(track, clipKey.clipId);
    if (!waveClip) {
        return;
    }

    const std::vector<double> channelHeight {
        params.geometry.height * params.channelHeightRatio,
        params.geometry.height * (1 - params.channelHeightRatio),
    };

    float zoomMin, zoomMax;
    const auto& cache = WaveformScale::Get(*track);
    cache.GetDisplayBounds(zoomMin, zoomMax);

    const auto& settings = WaveformSettings::Get(*track);
    const float dBRange = settings.dBRange;
    const bool dB = !settings.isLinear();
    const double trimLeft = waveClip->GetTrimLeft();

    auto waveMetrics = wavepainterutils::getWaveMetrics(globalContext()->currentProject(), clipKey, params);

    for (size_t index = 0; index < waveClip->NChannels(); index++) {
        waveMetrics.height = channelHeight[index];
        samplespainterutils::drawBackground(painter, waveMetrics, params.style, trimLeft);
        samplespainterutils::drawBaseLine(painter, waveMetrics, params.style);
        const auto samples = samplespainterutils::getSampleData(*waveClip, index, waveMetrics, dB, dBRange, zoomMax, zoomMin, 32);
        if (samples.size() == 0) {
            continue;
        }

        SampleData interpolatedSamples = sinc_interpolate(samples.y, samples.x, 16);
        //SampleData filteredSamples = apply_filter(interpolatedSamples, lowpassCoefs);

        int yZero = samplespainterutils::getWaveYPos(0.0, zoomMin, zoomMax, waveMetrics.height, dB, true, dBRange, false);
        yZero = waveMetrics.top + std::max(-1, std::min(static_cast<int>(waveMetrics.height + waveMetrics.top), yZero));

        //drawConnectingPoints(filteredSamples, waveMetrics, painter, params.style);
        drawConnectingPoints(interpolatedSamples, waveMetrics, painter, params.style);
        drawSampleHead(samples, waveMetrics, painter, params.style);
        //drawSampleStalk(samples, yZero, waveMetrics, painter, params.style);
        waveMetrics.top += waveMetrics.height;
    }
}
}
