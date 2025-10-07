#include "WaveformPainter.h"
#include "ZoomInfo.h"

namespace au::projectscene {
struct WaveBitmapCacheElementQt final : public WaveBitmapCacheElement
{
    uint8_t* Allocate(size_t width, size_t height) override
    {
        mWidth = width;
        mHeight = height;

        mBytes.resize(std::max(mWidth * mHeight * 3, mBytes.size()));
        return mBytes.data();
    }

    QImage GetImage() const
    {
        return QImage((const uchar*)mBytes.data(), mWidth, mHeight, mWidth * 3, QImage::Format_RGB888);
    }

    size_t Width() const override
    {
        return mWidth;
    }

    size_t Height() const override
    {
        return mHeight;
    }

private:
    size_t mWidth{};
    size_t mHeight{};
    std::vector<uint8_t> mBytes;
};

static au::au3::Au3WaveClip::Attachments::RegisteredFactory sKeyW{ [](au::au3::Au3WaveClip&) {
        return std::make_unique<WaveformPainter>();
    } };

WaveformPainter& WaveformPainter::Get(const au::au3::Au3WaveClip& clip)
{
    return const_cast< au::au3::Au3WaveClip& >(clip)   // Consider it mutable data
           .Attachments::Get<WaveformPainter>(sKeyW).EnsureClip(clip);
}

WaveformPainter& WaveformPainter::EnsureClip(const au::au3::Au3WaveClip& clip)
{
    const auto changed = mChanged.exchange(false);
    if (&clip != mWaveClip || changed) {
        mChannelCaches.clear();
    }

    const auto nChannels = clip.NChannels();

    if (mChannelCaches.size() == nChannels) {
        return *this;
    }

    mWaveClip = &clip;

    mChannelCaches.reserve(nChannels);

    for (size_t channelIndex = 0; channelIndex < nChannels; ++channelIndex) {
        auto dataCache = std::make_shared<WaveDataCache>(clip, channelIndex);

        auto bitmapCache = std::make_unique<WaveBitmapCache>(
            clip, dataCache,
            [] { return std::make_unique<WaveBitmapCacheElementQt>(); });

        mChannelCaches.push_back({ std::move(dataCache), std::move(bitmapCache) });
    }

    return *this;
}

void WaveformPainter::Draw(size_t channelIndex,
                           QPainter& painter,
                           const WavePaintParameters& params,
                           const au::projectscene::WaveMetrics& metrics)
{
    assert(channelIndex < mChannelCaches.size());
    if (channelIndex >= mChannelCaches.size()) {
        return;
    }

    auto& bitmapCache = mChannelCaches[channelIndex].BitmapCache;
    bitmapCache->SetPaintParameters(params);

    const ZoomInfo zoomInfo(0.0, metrics.zoom);
    bitmapCache->SetSelection(zoomInfo, metrics.selectionStartTime, metrics.selectionEndTime, true);

    auto range = bitmapCache->PerformLookup(zoomInfo, metrics.fromTime, metrics.toTime);

    double left = metrics.left;
    int height = metrics.height;

    for (auto it = range.begin(); it != range.end(); ++it) {
        const auto elementLeftOffset = it.GetLeftOffset();
        const auto elementRightOffset = it.GetRightOffset();

        const auto width = WaveBitmapCache::CacheElementWidth - elementLeftOffset - elementRightOffset;

        const auto drawableWidth = std::min<int32_t>(width, it->Width() - elementLeftOffset);

        const auto image = static_cast<const WaveBitmapCacheElementQt&>(*it).GetImage();
        painter.drawImage(
            QRectF(left, metrics.top, drawableWidth, height),
            image,
            QRectF(
                elementLeftOffset,
                0,
                std::clamp(drawableWidth, 0, image.width() - static_cast<int>(elementLeftOffset)),
                std::clamp(height, 0, image.height())
                )
            );

        left += width;
    }
}

void WaveformPainter::MarkChanged() noexcept
{
    mChanged.store(true);
}

void WaveformPainter::Invalidate()
{
    for (auto& channelCache : mChannelCaches) {
        channelCache.DataCache->Invalidate();
        channelCache.BitmapCache->Invalidate();
    }
}

std::unique_ptr<WaveClipListener> WaveformPainter::Clone() const
{
    return std::make_unique<WaveformPainter>();
}
}
