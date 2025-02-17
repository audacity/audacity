/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  WaveBitmapCache.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <memory>

#include "GraphicsDataCache.h"
#include "Observer.h"
#include "waveform/WavePaintParameters.h"

class wxBitmap;
class wxImage;
class WaveDataCache;
class Envelope;
class WaveClip;

//! An element, that contains a rasterized bitmap matching the WaveDataCacheElement
class WAVE_TRACK_PAINT_API WaveBitmapCacheElement : public GraphicsDataCacheElementBase
{
public:
    ~WaveBitmapCacheElement() override;

    virtual uint8_t* Allocate(size_t width, size_t height) = 0;

    virtual size_t Width() const = 0;
    virtual size_t Height() const = 0;

    size_t AvailableColumns { 0 };
};

//! Cache containing rasterized bitmaps representing the waveform
class WAVE_TRACK_PAINT_API WaveBitmapCache final : public GraphicsDataCache<WaveBitmapCacheElement>
{
public:
    WaveBitmapCache(const WaveClip& waveClip, std::shared_ptr<WaveDataCache> dataCache, ElementFactory elementFactory);
    ~WaveBitmapCache() override;

    WaveBitmapCache& SetPaintParameters(const WavePaintParameters& params);
    WaveBitmapCache& SetSelection(const ZoomInfo& zoomInfo, double t0, double t1, bool selected);

private:
    bool InitializeElement(
        const GraphicsDataCacheKey& key, WaveBitmapCacheElement& element) override;

    void CheckCache(const ZoomInfo&, double, double) override;

    struct LookupHelper;

    WavePaintParameters mPaintParamters;

    struct
    {
        int64_t FirstPixel { -1 };
        int64_t LastPixel { -1 };

        bool IsValid() const noexcept
        {
            return FirstPixel < LastPixel;
        }
    } mSelection;

    std::unique_ptr<LookupHelper> mLookupHelper;

    const Envelope* mEnvelope { nullptr };
    size_t mEnvelopeVersion { 0 };

    const WaveClip& mWaveClip;
    Observer::Subscription mStretchChangedSubscription;
};
