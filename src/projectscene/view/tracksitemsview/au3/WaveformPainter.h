#pragma once

#include <QPainter>
#include <memory>
#include <vector>

#include "au3-wave-track/WaveClip.h"
#include "au3-wave-track-paint/waveform/WavePaintParameters.h"
#include "au3-wave-track-paint/waveform/WaveDataCache.h"
#include "au3-wave-track-paint/waveform/WaveBitmapCache.h"

#include "au3wrap/au3types.h"
#include "WaveMetrics.h"
#include "../wavecolumn.h"

namespace au::projectscene {
class WaveformPainter final : public WaveClipListener
{
public:

    WaveformPainter();

    static WaveformPainter& Get(const au::au3::Au3WaveClip& cache);

    WaveformPainter& EnsureClip(const au::au3::Au3WaveClip& clip);
    void Draw(size_t channelIndex, QPainter& painter, const WavePaintParameters& params, const au::projectscene::WaveMetrics& metrics);

    //! Build vertex-ready column data from the WaveDataCache directly (bypassing bitmap cache).
    //! columns is cleared first; reusing it between calls keeps its capacity.
    //! dataComplete (optional) is set to false when any cache element is still being
    //! computed — the caller should request the data again later and repaint.
    void GetColumnData(size_t channelIndex, const WavePaintParameters& params, const au::projectscene::WaveMetrics& metrics,
                       std::vector<WaveColumnVertex>& columns, bool* dataComplete = nullptr);

    void MarkChanged() noexcept override;
    void Invalidate() override;
    std::unique_ptr<WaveClipListener> Clone() const override;

    //! Data epoch: changes whenever the underlying clip data or the caches serving
    //! it change, so views can detect that previously prepared output is stale even
    //! when all display inputs are unchanged. Values are unique across all clips.
    uint64_t Generation() const noexcept;

private:
    const au::au3::Au3WaveClip* mWaveClip {};

    struct ChannelCaches final
    {
        std::shared_ptr<WaveDataCache> DataCache;
        std::unique_ptr<WaveBitmapCache> BitmapCache;
    };

    std::vector<ChannelCaches> mChannelCaches;
    std::atomic<bool> mChanged = false;
    std::atomic<uint64_t> mGeneration;
};
}
