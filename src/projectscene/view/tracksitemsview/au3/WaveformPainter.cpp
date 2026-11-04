#include <QImage>
#include <cmath>
#include <limits>

#include "WaveformPainter.h"
#include "au3-screen-geometry/ZoomInfo.h"
#include "au3-mixer/Envelope.h"
#include "au3-utility/MemoryX.h"

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

namespace {
uint64_t nextGeneration() noexcept
{
    static std::atomic<uint64_t> next = 1;
    return next.fetch_add(1, std::memory_order_relaxed);
}
}

WaveformPainter::WaveformPainter()
    : mGeneration(nextGeneration())
{
}

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
        if (!mChannelCaches.empty()) {
            mGeneration.store(nextGeneration(), std::memory_order_relaxed);
        }
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

namespace {
//! NOTE: The data cache keeps its entries per exact zoom, so each step of a
//! zoom gesture recomputed every visible clip, reading block summaries from the
//! project file. Looking the data up at the next power of two instead lets all
//! steps within an octave share entries; the finer columns are then combined
//! down to the display zoom, which looks the same.
double dataCacheZoom(double zoom, double sampleRate)
{
    // Combining needs a few samples per data column: near sample level, look up as is
    constexpr double minSamplesPerDataColumn = 16.0;

    const double quantized = std::exp2(std::ceil(std::log2(zoom)));
    if (sampleRate / quantized < minSamplesPerDataColumn) {
        return zoom;
    }
    return quantized;
}
}

void WaveformPainter::GetColumnData(
    size_t channelIndex,
    const WavePaintParameters& params,
    const au::projectscene::WaveMetrics& metrics,
    std::vector<WaveColumnVertex>& columns,
    bool* dataComplete)
{
    columns.clear();

    if (dataComplete) {
        *dataComplete = true;
    }

    if (channelIndex >= mChannelCaches.size() || metrics.height <= 0 || metrics.zoom <= 0.0 || !mWaveClip) {
        return;
    }

    auto& dataCache = mChannelCaches[channelIndex].DataCache;
    dataCache->UpdateViewportWidth(static_cast<int64_t>(metrics.width));

    const ZoomInfo zoomInfo(0.0, metrics.zoom);
    const double sampleRate = mWaveClip->GetRate() / mWaveClip->GetStretchRatio();
    const double dataZoom = dataCacheZoom(metrics.zoom, sampleRate);
    const ZoomInfo dataZoomInfo(0.0, dataZoom);

    auto range = dataCache->PerformLookup(dataZoomInfo, metrics.fromTime, metrics.toTime);

    if (dataComplete) {
        *dataComplete = !(range.begin() == range.end() && metrics.width > 0);
    }

    // Display pixels of the window, as the lookup at the display zoom would give them
    const int64_t firstPixel = zoomInfo.TimeToPosition(metrics.fromTime);
    const int64_t lastPixel = zoomInfo.TimeToPosition(metrics.toTime);
    if (lastPixel < firstPixel) {
        return;
    }

    columns.reserve(static_cast<size_t>(lastPixel - firstPixel) + 1);

    const auto height = static_cast<float>(metrics.height);
    const auto displayMin = static_cast<float>(params.Min);
    const auto displayMax = static_cast<float>(params.Max);
    const bool dbScale = params.DBScale;
    const float dbRange = static_cast<float>(params.DBRange);
    const bool showClipping = params.ShowClipping;

    auto valueToY = [displayMin, displayMax, height](float value) -> float {
        if (displayMax == displayMin) {
            return height * 0.5f;
        }
        float normalized = (displayMax - value) / (displayMax - displayMin);
        return normalized * (height - 1);
    };

    auto applyDB = [dbRange](float value) -> float {
        float sign = (value >= 0 ? 1.0f : -1.0f);
        if (value != 0.0f) {
            float db = static_cast<float>(LINEAR_TO_DB(fabs(value)));
            value = (db + dbRange) / dbRange;
            if (value < 0.0f) {
                value = 0.0f;
            }
            value *= sign;
        }
        return value;
    };

    // Compute selection pixel range
    const int64_t selFirst = zoomInfo.TimeToPosition(metrics.selectionStartTime);
    const int64_t selLast = std::max(zoomInfo.TimeToPosition(metrics.selectionEndTime), selFirst + 1);

    // Get envelope if present
    const Envelope* envelope = params.AttachedEnvelope;
    const bool hasEnvelope = envelope != nullptr
                             && (envelope->GetNumberOfPoints() > 0
                                 || envelope->GetDefaultValue() != 1.0);

    //! NOTE: All callers run on the GUI thread; sharing the buffer keeps its capacity
    static std::vector<double> envValues;
    if (hasEnvelope) {
        envValues.resize(static_cast<size_t>(lastPixel - firstPixel) + 1);
        envelope->GetValues(
            envValues.data(), static_cast<int>(envValues.size()),
            firstPixel / metrics.zoom + envelope->GetOffset(),
            1.0 / metrics.zoom);
    }

    // PerformLookup snaps fromTime to a whole column; carry the discarded
    // sub-pixel fraction into the vertex positions so the waveform does not
    // wobble against the smoothly-moving clip edge while trimming/scrolling.
    const double xOrigin = metrics.left - metrics.fromTime * metrics.zoom;

    auto emitColumn = [&](int64_t pixel, WaveDisplayColumn columnData) {
        // Apply envelope
        if (hasEnvelope) {
            const float envVal = static_cast<float>(envValues[static_cast<size_t>(pixel - firstPixel)]);
            columnData.min *= envVal;
            columnData.max *= envVal;
            columnData.rms *= envVal;
        }

        // Apply dB scale
        if (dbScale) {
            columnData.min = applyDB(columnData.min);
            columnData.max = applyDB(columnData.max);
            columnData.rms = applyDB(columnData.rms);
        }

        const bool clipping = showClipping
                              && (columnData.min <= static_cast<float>(-MAX_AUDIO)
                                  || columnData.max >= static_cast<float>(MAX_AUDIO));

        WaveColumnVertex v;
        v.x = static_cast<float>(xOrigin + static_cast<double>(pixel));
        v.maxY = valueToY(columnData.max);
        v.minY = valueToY(columnData.min);
        if (v.minY - v.maxY < 1.0f) {
            const float center = 0.5f * (v.maxY + v.minY);
            v.maxY = center - 0.5f;
            v.minY = center + 0.5f;
        }
        v.rmsMaxY = valueToY(std::min(columnData.rms, columnData.max));
        v.rmsMinY = valueToY(std::max(-columnData.rms, columnData.min));
        v.selected = pixel >= selFirst && pixel < selLast;
        v.clipping = clipping;

        columns.push_back(v);
    };

    // Data columns falling into the same display pixel are combined
    int64_t pendingPixel = std::numeric_limits<int64_t>::min();
    float pendingMin = 0.0f;
    float pendingMax = 0.0f;
    double pendingSquares = 0.0;
    int pendingCount = 0;

    auto flushPending = [&]() {
        if (pendingCount == 0) {
            return;
        }
        const float rms = static_cast<float>(std::sqrt(pendingSquares / pendingCount));
        emitColumn(pendingPixel, { pendingMin, pendingMax, rms });
        pendingCount = 0;
    };

    const double displayPerDataColumn = metrics.zoom / dataZoom;
    constexpr size_t tileWidth = GraphicsDataCacheBase::CacheElementWidth;

    int64_t tileStartColumn = dataZoomInfo.TimeToPosition(metrics.fromTime);

    for (auto it = range.begin(); it != range.end(); ++it) {
        const auto& element = *it;
        const size_t leftOffset = it.GetLeftOffset();
        const size_t rightOffset = it.GetRightOffset();

        //! NOTE: Columns are placed by their index, like the bitmap renderer:
        //! an incomplete tile must leave a gap, not shift the rest.
        const size_t nominalWidth = tileWidth - leftOffset - rightOffset;
        const size_t endCol = std::min<size_t>(element.AvailableColumns, tileWidth - rightOffset);

        // The final element of a clip is never IsComplete (it nominally spans
        // past the clip's end); only report incompleteness when columns are
        // missing within the requested window.
        if (dataComplete && !element.IsComplete
            && element.AvailableColumns < tileWidth - rightOffset) {
            *dataComplete = false;
        }

        for (size_t col = leftOffset; col < endCol; ++col) {
            const int64_t dataColumn = tileStartColumn + static_cast<int64_t>(col - leftOffset);
            // A data column belongs to the display pixel its centre falls into
            const auto pixel = static_cast<int64_t>(std::floor((dataColumn + 0.5) * displayPerDataColumn));
            if (pixel < firstPixel || pixel > lastPixel) {
                continue;
            }

            const auto& data = element.Data[col];
            if (pixel != pendingPixel) {
                flushPending();
                pendingPixel = pixel;
                pendingMin = data.min;
                pendingMax = data.max;
                pendingSquares = 0.0;
            } else {
                pendingMin = std::min(pendingMin, data.min);
                pendingMax = std::max(pendingMax, data.max);
            }
            pendingSquares += static_cast<double>(data.rms) * data.rms;
            ++pendingCount;
        }

        tileStartColumn += static_cast<int64_t>(nominalWidth);
    }

    flushPending();
}

void WaveformPainter::MarkChanged() noexcept
{
    mChanged.store(true);
    mGeneration.store(nextGeneration(), std::memory_order_relaxed);
}

void WaveformPainter::Invalidate()
{
    for (auto& channelCache : mChannelCaches) {
        channelCache.DataCache->Invalidate();
        channelCache.BitmapCache->Invalidate();
    }

    mGeneration.store(nextGeneration(), std::memory_order_relaxed);
}

uint64_t WaveformPainter::Generation() const noexcept
{
    return mGeneration.load(std::memory_order_relaxed);
}

std::unique_ptr<WaveClipListener> WaveformPainter::Clone() const
{
    return std::make_unique<WaveformPainter>();
}
}
