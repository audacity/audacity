/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  WaveClipPainter.cpp

  Dmitry Vedenko

**********************************************************************/
#include "WaveClipPainter.h"

#include "WaveDataCache.h"
#include "WaveBitmapCache.h"

//#include "graphics/Painter.h"

namespace
{
class WaveClipBitmapPainter final : public WaveClipPainter
{
public:
   explicit WaveClipBitmapPainter(std::shared_ptr<WaveDataCache> dataCache)
       : mBitmapCache(std::move(dataCache), dataCache->GetSampleRate())
   {

   }

   void SetSelection(const ZoomInfo& zoomInfo, float t0, float t1) override
   {
      mBitmapCache.SetSelection(zoomInfo, t0, t1);
   }

   void Draw(
      graphics::Painter& painter, const WavePaintParameters& params,
      const ZoomInfo& zoomInfo, const graphics::Rect& targetRect,
      float leftOffset, float from, float to) override
   {
      mBitmapCache.SetPainter(painter).SetPaintParameters(params);

      auto range = mBitmapCache.PerformLookup(zoomInfo, from, to);

      auto left = targetRect.origin.x + leftOffset;
      auto height = targetRect.size.height;

      auto clipStateMutator = painter.GetClipStateMutator();
      clipStateMutator.SetClipRect(targetRect, false);

      const auto top = targetRect.origin.y;

      for (auto it = range.begin(); it != range.end(); ++it)
      {
         const auto elementLeftOffset = it.GetLeftOffset();
         const auto elementRightOffset = it.GetRightOffset();
         const auto width = WaveBitmapCache::CacheElementWidth -
                            elementLeftOffset - elementRightOffset;

         const auto drawableWidth = std::min<int32_t>(
            width, it->Bitmap->GetWidth() - elementLeftOffset);

         painter.DrawImage(
            *it->Bitmap, left, targetRect.origin.y, drawableWidth, height,
            elementLeftOffset, 0);

         left += width;
      }
   }

private:
   WaveBitmapCache mBitmapCache;
};

} // namespace

/*WAVE_TRACK_PAINT_API std::shared_ptr<WaveClipPainter> CreateWaveClipPainter(
   const graphics::RendererID&,
   std::shared_ptr<WaveDataCache> dataCache)
{
   if (dataCache == nullptr)
      return {};
   
   return std::make_shared<WaveClipBitmapPainter>(std::move(dataCache));
}*/
