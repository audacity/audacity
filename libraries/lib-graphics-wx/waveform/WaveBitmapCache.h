/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  WaveBitmapCache.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <cmath>
#include <memory>
#include <numeric>

#include "GraphicsDataCache.h"

#include "graphics/Color.h"
#include "waveform/WavePaintParameters.h"

class wxBitmap;
class wxImage;
class WaveDataCache;
class Envelope;

struct GRAPHICS_WX_API WaveBitmapCacheElement final :
    GraphicsDataCacheElementBase
{
   ~WaveBitmapCacheElement();

   void Dispose() override;

   std::unique_ptr<wxBitmap> Bitmap;
   size_t AvailableColumns { 0 };
};

class GRAPHICS_WX_API WaveBitmapCache final :
    public GraphicsDataCache<WaveBitmapCacheElement>
{
public:
   WaveBitmapCache(std::shared_ptr<WaveDataCache> dataCache, double sampleRate);
   ~WaveBitmapCache();

   WaveBitmapCache& SetPaintParameters(const WavePaintParameters& params);
   WaveBitmapCache& SetSelection(const ZoomInfo& zoomInfo, double t0, double t1);

private:
   bool InitializeElement(
      const GraphicsDataCacheKey& key, WaveBitmapCacheElement& element) override;

   void CheckCache(const ZoomInfo&, double, double) override;

private:
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
 
   std::unique_ptr<wxImage> mCachedImage;
   std::unique_ptr<LookupHelper> mLookupHelper;

   const Envelope* mEnvelope { nullptr };
   size_t mEnvelopeVersion { 0 };
};
