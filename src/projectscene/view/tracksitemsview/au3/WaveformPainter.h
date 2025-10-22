#pragma once

#include "au3wrap/au3types.h"
#include "WaveClip.h"
#include "WaveMetrics.h"
#include "waveform/WavePaintParameters.h"
#include "waveform/WaveDataCache.h"
#include "waveform/WaveBitmapCache.h"

namespace au::projectscene {
class WaveformPainter final : public WaveClipListener
{
public:

    static WaveformPainter& Get(const au::au3::Au3WaveClip& cache);

    WaveformPainter& EnsureClip(const au::au3::Au3WaveClip& clip);
    void Draw(size_t channelIndex, QPainter& painter, const WavePaintParameters& params, const au::projectscene::WaveMetrics& metrics);
    void MarkChanged() noexcept override;
    void Invalidate() override;
    std::unique_ptr<WaveClipListener> Clone() const override;

private:
    const au::au3::Au3WaveClip* mWaveClip {};

    struct ChannelCaches final
    {
        std::shared_ptr<WaveDataCache> DataCache;
        std::unique_ptr<WaveBitmapCache> BitmapCache;
    };

    std::vector<ChannelCaches> mChannelCaches;
    std::atomic<bool> mChanged = false;
};
}
