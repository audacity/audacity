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

   WaveBitmapCache& SetDisplayParameters(int height, double zoomMin, double zoomMax, bool showClipping);
   WaveBitmapCache& SetDBParameters(double dbRange, bool dbScale);
   WaveBitmapCache& SetSelection(const ZoomInfo& zoomInfo, double t0, double t1);
   WaveBitmapCache& SetBlankColor(Color color);
   WaveBitmapCache& SetBackgroundColors(Color normal, Color selected);
   WaveBitmapCache& SetSampleColors(Color normal, Color selected);
   WaveBitmapCache& SetRMSColors(Color normal, Color selected);
   WaveBitmapCache& SetClippingColors(Color normal, Color selected);
   WaveBitmapCache& SetEnvelope(const Envelope& envelope);

private:
   bool InitializeElement(
      const GraphicsDataCacheKey& key, WaveBitmapCacheElement& element) override;

private:
   struct LookupHelper;

   struct ColorPair final
   {
      Color Normal;
      Color Selected;
   };

   int mHeight { 0 };

   double mMin { -1.0 };
   double mMax { 1.0 };

   double mDBRange { 60.0 };

   struct  
   {
      int64_t FirstPixel { -1 };
      int64_t LastPixel { -1 };

      bool IsValid() const noexcept
      {
         return FirstPixel < LastPixel;
      }
   } mSelection;

   Color mBlankColor;

   ColorPair mBackgroundColors;
   ColorPair mSampleColors;
   ColorPair mRMSColors;
   ColorPair mClippingColors;
   
   std::unique_ptr<wxImage> mCachedImage;
   std::unique_ptr<LookupHelper> mLookupHelper;

   const Envelope* mEnvelope { nullptr };
   size_t mEnvelopeVersion { 0 };

   bool mShowClipping { false };
   bool mDBScale { false };
};
