/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  WaveBitmapCache.h

  Dmitry Vedenko

**********************************************************************/
#include "WaveBitmapCache.h"

#include <cassert>
#include <cmath>
#include <cstring>

#include "ZoomInfo.h"

#include "waveform/WaveDataCache.h"

#include "Envelope.h"
#include "FrameStatistics.h"

#include "WaveClip.h"

// The worst case scenario is:
// blank > background > envelope top + envelope bottom > sample > rms > zero line
// So we have 7 bands

constexpr size_t ColorFunctionBands = 7;

struct Triplet final
{
    Triplet() = default;

    explicit Triplet(graphics::Color color)
        : r(color.GetRed())
        , g(color.GetGreen())
        , b(color.GetBlue())
    {
    }

    void SetColor(graphics::Color color)
    {
        r = color.GetRed();
        g = color.GetGreen();
        b = color.GetBlue();
    }

    Triplet(const Triplet&) = default;
    Triplet(Triplet&&) = default;
    Triplet& operator=(const Triplet&) = default;
    Triplet& operator=(Triplet&&) = default;

    uint8_t r { 0 };
    uint8_t g { 0 };
    uint8_t b { 0 };

    uint8_t unused { 0 };
};

struct Band final
{
    Triplet color;
    int from;
    int to;
};

struct ColorFunction final
{
    std::array<Band, ColorFunctionBands> bands;
    size_t bandsCount { 0 };

    Triplet GetColor(int row, Triplet defaultColor) const noexcept
    {
        // Later bands are the topmost
        for (size_t i = bandsCount; i > 0; --i) {
            const Band& band = bands[i - 1];
            if (band.from <= row && row < band.to) {
                return band.color;
            }
        }
        return defaultColor;
    }

    void AddBand(graphics::Color color, int from, int to)
    {
        assert(bandsCount < bands.size());
        assert(from <= to);

        bands[bandsCount++] = { Triplet(color), from, to };
    }

    void Reset()
    {
        bandsCount = 0;
    }
};
struct WaveBitmapCache::LookupHelper final
{
    explicit LookupHelper(std::shared_ptr <WaveDataCache> dataCache)
        : DataCache(std::move(dataCache))
    {
    }

    bool PerformLookup(WaveBitmapCache* cache, GraphicsDataCacheKey key)
    {
        DataCache->UpdateViewportWidth(cache->GetMaxViewportWidth());

        auto result = DataCache->PerformLookup(key);

        if (result == nullptr) {
            return false;
        }

        auto sw = FrameStatistics::CreateStopwatch(
            FrameStatistics::SectionID::WaveBitmapCachePreprocess);

        const auto columnsCount = result->AvailableColumns;

        if (cache->mPaintParameters.DBScale) {
            auto GetDBValue
                =[dbRange = cache->mPaintParameters.DBRange](float value)
            {
                float sign = (value >= 0 ? 1 : -1);

                if (value != 0.) {
                    float db = LINEAR_TO_DB(fabs(value));
                    value = (db + dbRange) / dbRange;

                    if (value < 0.0) {
                        value = 0.0;
                    }

                    value *= sign;
                }

                return value;
            };

            for (size_t column = 0; column < columnsCount; ++column) {
                auto oldColumn = result->Data[column];

                DBRemappedColumns[column] = { GetDBValue(oldColumn.min),
                                              GetDBValue(oldColumn.max),
                                              GetDBValue(oldColumn.rms) };
            }
        }

        auto GetRowFromValue
            =[min = cache->mPaintParameters.Min, max = cache->mPaintParameters.Max,
              height = cache->mPaintParameters.Height](float value)
        {
            value = (max - value) / (max - min);
            return static_cast<int>(value * (height - 1) + 0.5);
        };

        const auto height = cache->mPaintParameters.Height;

        const auto zeroLineY = GetRowFromValue(.0f);

        auto inputData = cache->mPaintParameters.DBScale
                         ? DBRemappedColumns.data()
                         : result->Data.data();

        auto envelope = cache->mEnvelope;

        if (
            envelope != nullptr && (envelope->GetNumberOfPoints() > 0
                                    || envelope->GetDefaultValue() != 1.0)) {
            envelope->GetValues(
                EnvelopeValues.data(), static_cast<int>(EnvelopeValues.size()),
                key.FirstSample / cache->GetScaledSampleRate()
                + envelope->GetOffset(),
                1.0 / key.PixelsPerSecond);

            for (size_t column = 0; column < columnsCount; ++column) {
                const auto columnData = inputData[column];
                const float envelopeValue = EnvelopeValues[column];

                EnvRemappedColumns[column] = {
                    columnData.min* envelopeValue,
                    columnData.max* envelopeValue,
                    columnData.rms* envelopeValue
                };
            }

            inputData = EnvRemappedColumns.data();
        }

        const auto globalMaxRow = GetRowFromValue(cache->mPaintParameters.Max);
        const auto globalMinRow = GetRowFromValue(cache->mPaintParameters.Min) + 1;

        const auto blankColor = cache->mPaintParameters.BlankColor;
        const auto zeroLineColor  = cache->mPaintParameters.ZeroLineColor;

        const auto backgroundColors = cache->mPaintParameters.BackgroundColors;
        const auto envelopeColors = cache->mPaintParameters.EnvelopeColors;
        const auto sampleColors = cache->mPaintParameters.SampleColors;
        const auto rmsColors = cache->mPaintParameters.RMSColors;
        const auto clipColors = cache->mPaintParameters.ClippingColors;
        const auto showRMS = cache->mPaintParameters.ShowRMS;
        const auto drawEnvelope = cache->mPaintParameters.DrawEnvelope;

        auto firstPixel = int64_t(key.FirstSample / cache->GetScaledSampleRate() * key.PixelsPerSecond + 0.5);

        const auto selFirst = cache->mSelection.FirstPixel;
        const auto selLast = cache->mSelection.LastPixel;

        const bool showClipping = cache->mPaintParameters.ShowClipping;

        for (size_t column = 0; column < columnsCount; ++column) {
            const bool selected = firstPixel >= selFirst && firstPixel < selLast;
            ++firstPixel;

            const auto columnData = inputData[column];
            auto& function = ColorFunctions[column];
            function.Reset();

            if (showClipping && (columnData.min <= -MAX_AUDIO || columnData.max >= MAX_AUDIO)) {
                function.AddBand(selected ? clipColors.Selected : clipColors.Normal, 0, height);
                continue;
            }

            const auto maxSampleRow = GetRowFromValue(columnData.max);
            const auto minSampleRow = GetRowFromValue(columnData.min);

            function.AddBand(selected ? backgroundColors.Selected : backgroundColors.Normal, globalMaxRow, globalMinRow);

            if (envelope) {
                const auto envelopeValue = envelope->GetNumberOfPoints() ? EnvelopeValues[column] : envelope->GetDefaultValue();
                const auto envelopePositiveRow = std::clamp(GetRowFromValue(envelopeValue), globalMaxRow, globalMinRow);
                const auto envelopeNegativeRow = std::clamp(GetRowFromValue(-envelopeValue) + 1, globalMaxRow, globalMinRow);

                function.AddBand(envelopeColors.Normal, globalMaxRow, envelopePositiveRow);

                if (drawEnvelope) {
                    const auto envelopeEndPositiveRow = std::clamp(GetRowFromValue(envelopeValue - 0.5), globalMaxRow, zeroLineY);
                    const auto envelopeEndNegativeRow = std::clamp(GetRowFromValue(-envelopeValue + 0.5) + 1, zeroLineY, globalMinRow);

                    if (envelopeEndNegativeRow != envelopeEndPositiveRow) {
                        function.AddBand(envelopeColors.Normal, envelopeEndPositiveRow, envelopeEndNegativeRow);
                    }
                }

                function.AddBand(envelopeColors.Normal, envelopeNegativeRow, globalMinRow);
            }

            function.AddBand(zeroLineColor, zeroLineY, zeroLineY + 1);

            function.AddBand(selected ? sampleColors.Selected : sampleColors.Normal, maxSampleRow,
                             minSampleRow == maxSampleRow ? minSampleRow + 1 : minSampleRow);

            if (showRMS) {
                const auto rmsPositiveRow = GetRowFromValue(std::min(columnData.rms, columnData.max));
                const auto rmsNegativeRow = GetRowFromValue(std::max(-columnData.rms, columnData.min));

                if (rmsNegativeRow >= rmsPositiveRow) {
                    function.AddBand(selected ? rmsColors.Selected : rmsColors.Normal, rmsPositiveRow, rmsNegativeRow);
                }
            }
        }

        AvailableColumns = columnsCount;
        IsComplete = result->IsComplete;

        return true;
    }

    std::shared_ptr<WaveDataCache> DataCache;

    std::array<ColorFunction, GraphicsDataCacheBase::CacheElementWidth>
    ColorFunctions {};

    WaveCacheElement::Columns DBRemappedColumns {};

    std::array<double, GraphicsDataCacheBase::CacheElementWidth>
    EnvelopeValues {};
    WaveCacheElement::Columns EnvRemappedColumns {};

    size_t AvailableColumns { 0 };
    bool IsComplete { 0 };
};

WaveBitmapCache::WaveBitmapCache(
    const WaveClip& waveClip, std::shared_ptr<WaveDataCache> dataCache, ElementFactory elementFactory)
    : GraphicsDataCache{waveClip.GetRate() / waveClip.GetStretchRatio(),
                        std::move(elementFactory)}
    , mLookupHelper{std::make_unique<LookupHelper>(std::move(dataCache))}
    , mWaveClip{waveClip}
    , mStretchChangedSubscription{
                                  const_cast<WaveClip&>(waveClip)
                                  .Observer::Publisher<StretchRatioChange>::Subscribe(
                                      [this](const StretchRatioChange&) {
        SetScaledSampleRate(
            mWaveClip.GetRate() / mWaveClip.GetStretchRatio());
    })
                                  }
{
}

WaveBitmapCache::~WaveBitmapCache() = default;

WaveBitmapCacheElement::~WaveBitmapCacheElement() = default;

WaveBitmapCache&
WaveBitmapCache::SetPaintParameters(const WavePaintParameters& params)
{
    if (mPaintParameters != params) {
        mPaintParameters = params;
        mEnvelope = params.AttachedEnvelope;
        mEnvelopeVersion = mEnvelope != nullptr ? mEnvelope->GetVersion() : 0;

        Invalidate();
    }

    return *this;
}

WaveBitmapCache& WaveBitmapCache::SetSelection(
    const ZoomInfo& zoomInfo, double t0, double t1, bool selected)
{
    const auto empty = !selected || t0 > t1
                       || ((t1 - t0) < std::numeric_limits<double>::epsilon());

    const auto first = empty ? int64_t(-1) : zoomInfo.TimeToPosition(t0);
    const auto last
        =empty ? int64_t(-1) : std::max(zoomInfo.TimeToPosition(t1), first + 1);

    if (mSelection.FirstPixel != first || mSelection.LastPixel != last) {
        mSelection.FirstPixel = first;
        mSelection.LastPixel = last;

        Invalidate();
    }

    return *this;
}

void WaveBitmapCache::CheckCache(const ZoomInfo&, double, double)
{
    if (mEnvelope != nullptr && mEnvelopeVersion != mEnvelope->GetVersion()) {
        mEnvelopeVersion = mEnvelope->GetVersion();
        Invalidate();
    }
}

bool WaveBitmapCache::InitializeElement(
    const GraphicsDataCacheKey& key, WaveBitmapCacheElement& element)
{
    if (mPaintParameters.Height == 0) {
        return false;
    }

    if (!mLookupHelper->PerformLookup(this, key)) {
        const auto width = 1;
        const auto height = mPaintParameters.Height;
        const auto bytes = element.Allocate(width, height);
        std::memset(bytes, 0, width * height * 3);
        return true;
    }

    auto sw = FrameStatistics::CreateStopwatch(
        FrameStatistics::SectionID::WaveBitmapCache);

    const auto columnsCount = mLookupHelper->AvailableColumns;

    const auto defaultColor = Triplet(mPaintParameters.BlankColor);

    const auto height = static_cast<int>(mPaintParameters.Height);

    auto rowData = element.Allocate(columnsCount, height);

    for (int row = 0; row < height; ++row) {
        auto colorFunction = mLookupHelper->ColorFunctions.data();

        for (size_t pixel = 0; pixel < columnsCount; ++pixel) {
            const auto color = colorFunction->GetColor(row, defaultColor);

            *rowData++ = color.r;
            *rowData++ = color.g;
            *rowData++ = color.b;

            ++colorFunction;
        }
    }

    element.AvailableColumns = columnsCount;
    element.IsComplete = mLookupHelper->IsComplete;

    return true;
}
