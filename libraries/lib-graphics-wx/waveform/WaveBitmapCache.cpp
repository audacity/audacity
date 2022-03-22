/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  WaveBitmapCache.h

  Dmitry Vedenko

**********************************************************************/
#include "WaveBitmapCache.h"

#include <cassert>

#include <wx/bitmap.h>
#include <wx/image.h>

#include "ZoomInfo.h"

#include "waveform/WaveDataCache.h"

#include "Envelope.h"
#include "FrameStatistics.h"

// The worst case scenario is:
// blank -> background -> sample -> rms -> sample -> backgroud -> blank
// So we have 7 stops

constexpr size_t ColorFunctionStops = 7;

struct Triplet final
{
   Triplet() = default;

   explicit Triplet(Color color)
       : r(color.GetRed())
       , g(color.GetGreen())
       , b(color.GetBlue())
   {
   }

   void SetColor(Color color)
   {
      r = color.GetRed();
      g = color.GetGreen();
      b = color.GetBlue();
   }

   Triplet(const Triplet&) = default;
   Triplet(Triplet&&) = default;

   uint8_t r { 0 };
   uint8_t g { 0 };
   uint8_t b { 0 };

   uint8_t unused { 0 };
};

struct ColorFunction final
{
   std::array<std::pair<Triplet, uint32_t>, ColorFunctionStops> Stops;

   Triplet GetColor(uint32_t row, Triplet defaultColor) const noexcept
   {
      for (auto stop : Stops)
      {
         if (row < stop.second)
            return stop.first;
      }

      return defaultColor;
   }

   void SetStop(size_t index, Color color, uint32_t position)
   {
      assert(index < Stops.size());

      Stops[index].first.SetColor(color);
      Stops[index].second = position;
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
      auto result = DataCache->PerformLookup(key);

      if (result == nullptr)
         return false;

      auto sw = FrameStatistics::CreateStopwatch(
         FrameStatistics::SectionID::WaveBitmapCachePreprocess);

      const auto columnsCount = result->AvailableColumns;

      if (cache->mDBScale)
      {
         auto GetDBValue = [dbRange = cache->mDBRange](float value)
         {
            float sign = (value >= 0 ? 1 : -1);

            if (value != 0.)
            {
               float db = LINEAR_TO_DB(fabs(value));
               value = (db + dbRange) / dbRange;

               if (value < 0.0)
                  value = 0.0;

               value *= sign;
            }

            return value;
         };

         for (size_t column = 0; column < columnsCount; ++column)
         {
            auto oldColumn = result->Data[column];

            DBRemappedColumns[column] = { GetDBValue(oldColumn.min),
                                        GetDBValue(oldColumn.max),
                                        GetDBValue(oldColumn.rms) };
         }
      }

      auto GetRowFromValue = [min = cache->mMin, max = cache->mMax,
                              height = cache->mHeight,
                              dbRange = cache->mDBRange](float value)
      {
         value = (max - value) / (max - min);
         return static_cast<int>(value * (height - 1) + 0.5);
      };

      const auto height = cache->mHeight;

      auto inputData =
         cache->mDBScale ? DBRemappedColumns.data() : result->Data.data();

      auto envelope = cache->mEnvelope;

      if (
         envelope != nullptr && (envelope->GetNumberOfPoints() > 0 ||
                                 envelope->GetDefaultValue() != 1.0))
      {
         envelope->GetValues(
            EnvelopeValues.data(), static_cast<int>(EnvelopeValues.size()),
            key.FirstSample / cache->GetSampleRate(),
            1.0 / key.PixelsPerSecond);

         for (size_t column = 0; column < columnsCount; ++column)
         {
            const auto columnData = inputData[column];
            const float envelopeValue = EnvelopeValues[column];

            EnvRemappedColumns[column] = {
               columnData.min * envelopeValue,
               columnData.max * envelopeValue,
               columnData.rms * envelopeValue
            };
         }

         inputData = EnvRemappedColumns.data();
      }

      const bool hasTopBlankArea = cache->mMax > 1.0;
      const auto globalMaxRow = GetRowFromValue(cache->mMax);
      const auto globalMinRow = GetRowFromValue(cache->mMin) + 1;

      const auto blankColor = cache->mBlankColor;

      const auto backgroundColors = cache->mBackgroundColors;
      const auto sampleColors = cache->mSampleColors;
      const auto rmsColors = cache->mRMSColors;
      const auto clipColors = cache->mClippingColors;

      auto firstPixel = int64_t(key.FirstSample / cache->GetSampleRate() * key.PixelsPerSecond + 0.5);

      const auto selFirst = cache->mSelection.FirstPixel;
      const auto selLast = cache->mSelection.LastPixel;

      const bool showClipping = cache->mShowClipping;

      for (size_t column = 0; column < columnsCount; ++column)
      {
         const bool selected = firstPixel >= selFirst && firstPixel < selLast;
         ++firstPixel;

         const auto columnData = inputData[column];
         auto& function = ColorFunctions[column];

         if (showClipping && (columnData.min <= -MAX_AUDIO || columnData.max >= MAX_AUDIO))
         {
            function.SetStop(
               0, selected ? clipColors.Selected : clipColors.Normal, height);

            continue;
         }

         size_t stopIndex = 0;

         if (hasTopBlankArea)
            function.SetStop(stopIndex++, blankColor, globalMaxRow);

         const auto maxRow = GetRowFromValue(columnData.max);

         if (maxRow > 0)
         {
            function.SetStop(
               stopIndex++,
               selected ? backgroundColors.Selected : backgroundColors.Normal,
               maxRow);
         }

         if (maxRow >= height)
            continue;

         const auto positiveRMSRow = GetRowFromValue(columnData.rms);

         if (maxRow < positiveRMSRow)
         {
            function.SetStop(
               stopIndex++,
               selected ? sampleColors.Selected : sampleColors.Normal,
               positiveRMSRow);
         }

         if (positiveRMSRow >= height)
            continue;

         const auto negativeRMSRow = GetRowFromValue(-columnData.rms);

         if (positiveRMSRow < negativeRMSRow)
         {
            function.SetStop(
               stopIndex++, selected ? rmsColors.Selected : rmsColors.Normal,
               negativeRMSRow);
         }

         if (negativeRMSRow >= height)
            continue;

         const auto minRow = GetRowFromValue(columnData.min);

         function.SetStop(
            stopIndex++, selected ? sampleColors.Selected : sampleColors.Normal,
            minRow);

         if (minRow < globalMinRow)
         {
            function.SetStop(
               stopIndex++,
               selected ? backgroundColors.Selected : backgroundColors.Normal,
               globalMinRow);
         }

         if (globalMinRow < height)
            function.SetStop(stopIndex++, blankColor, height);
      }

      AvailableColumns = columnsCount;
      IsComplete = result->IsComplete;

      return true;
   }

   std::shared_ptr<WaveDataCache> DataCache;

   std::array<ColorFunction, GraphicsDataCacheBase::CacheElementWidth>
      ColorFunctions;

   WaveCacheElement::Columns DBRemappedColumns;

   std::array<double, GraphicsDataCacheBase::CacheElementWidth> EnvelopeValues; 
   WaveCacheElement::Columns EnvRemappedColumns;

   size_t AvailableColumns { 0 };
   bool IsComplete { 0 };
};

WaveBitmapCache::WaveBitmapCache(
   std::shared_ptr<WaveDataCache> dataCache, double sampleRate)
    : GraphicsDataCache(sampleRate)
    , mLookupHelper(std::make_unique<LookupHelper>(std::move(dataCache)))
{
}


WaveBitmapCache::~WaveBitmapCache()
{
}

WaveBitmapCacheElement::~WaveBitmapCacheElement()
{
}

void WaveBitmapCacheElement::Dispose()
{
   Bitmap = {};
}

WaveBitmapCache& WaveBitmapCache::SetDisplayParameters(
   int height, double min, double max, bool showClipping)
{
   const bool needsUpdate =
      height != mHeight ||
      showClipping != mShowClipping ||
      std::abs(min - mMin) > std::numeric_limits<float>::epsilon() ||
      std::abs(max - mMax) > std::numeric_limits<float>::epsilon();

   if (height != mHeight)
      mCachedImage = {};

   if (needsUpdate)
   {
      mHeight = height;
      mMin = min;
      mMax = max;

      mShowClipping = showClipping;

      Invalidate();
   }

   return *this;
}

WaveBitmapCache& WaveBitmapCache::SetDBParameters(double dbRange, bool dbScale)
{
   const bool needsUpdate =
      dbScale != mDBScale ||
      (dbScale &&  std::abs(dbRange - mDBRange) > std::numeric_limits<float>::epsilon());


   if (needsUpdate)
   {
      mDBRange = dbRange;
      mDBScale = dbScale;

      Invalidate();
   }

   return *this;
}

WaveBitmapCache& WaveBitmapCache::SetSelection(
   const ZoomInfo& zoomInfo, double t0, double t1)
{
   const auto empty =
      t0 > t1 || ((t1 - t0) < std::numeric_limits<double>::epsilon());

   const auto first = empty ? int64_t(-1) : zoomInfo.TimeToPosition(t0);
   const auto last =
      empty ? int64_t(-1) : std::max(zoomInfo.TimeToPosition(t1), first + 1);

   const bool needsUpdate =
      mSelection.FirstPixel != first || mSelection.LastPixel != last;

   if (needsUpdate)
   {
      mSelection.FirstPixel = first;
      mSelection.LastPixel = last;

      Invalidate();
   }

   return *this;
}

WaveBitmapCache& WaveBitmapCache::SetBlankColor(Color color)
{
   if (color != mBlankColor)
   {
      mBlankColor = color;
      Invalidate();
   }

   return *this;
}

WaveBitmapCache&
WaveBitmapCache::SetBackgroundColors(Color normal, Color selected)
{
   if (
      normal != mBackgroundColors.Normal ||
      selected != mBackgroundColors.Selected)
   {
      mBackgroundColors.Normal = normal;
      mBackgroundColors.Selected = selected;

      Invalidate();
   }

   return *this;
}

WaveBitmapCache& WaveBitmapCache::SetSampleColors(Color normal, Color selected)
{
   if (normal != mSampleColors.Normal || selected != mSampleColors.Selected)
   {
      mSampleColors.Normal = normal;
      mSampleColors.Selected = selected;

      Invalidate();
   }

   return *this;
}

WaveBitmapCache& WaveBitmapCache::SetRMSColors(Color normal, Color selected)
{
   if (normal != mRMSColors.Normal || selected != mRMSColors.Selected)
   {
      mRMSColors.Normal = normal;
      mRMSColors.Selected = selected;

      Invalidate();
   }

   return *this;
}

WaveBitmapCache& WaveBitmapCache::SetClippingColors(Color normal, Color selected)
{
   if (normal != mClippingColors.Normal || selected != mClippingColors.Selected)
   {
      mClippingColors.Normal = normal;
      mClippingColors.Selected = selected;

      Invalidate();
   }

   return *this;
}

WaveBitmapCache& WaveBitmapCache::SetEnvelope(const Envelope& envelope)
{
   if (mEnvelope != &envelope)
   {
      mEnvelope = &envelope;
      mEnvelopeVersion = mEnvelope->GetVersion();
      Invalidate();
   }
   else if (mEnvelopeVersion != mEnvelope->GetVersion())
   {
      mEnvelopeVersion = mEnvelope->GetVersion();
      Invalidate();
   }

   return *this;
}

bool WaveBitmapCache::InitializeElement(
   const GraphicsDataCacheKey& key, WaveBitmapCacheElement& element)
{
   if (mHeight == 0)
      return false;

   if (!mCachedImage)
      mCachedImage = std::make_unique<wxImage>(CacheElementWidth, mHeight);

   if (!mLookupHelper->PerformLookup(this, key))
      return false;

   auto sw = FrameStatistics::CreateStopwatch(
      FrameStatistics::SectionID::WaveBitmapCache);

   auto imageData = mCachedImage->GetData();
   constexpr auto stride = CacheElementWidth * 3;

   const auto columnsCount = mLookupHelper->AvailableColumns;

   const auto defaultColor = Triplet(mBlankColor);

   const auto height = static_cast<uint32_t>(mHeight);

   for (uint32_t row = 0; row < height; ++row)
   {
      auto rowData = imageData + row * stride;
      auto colorFunction = mLookupHelper->ColorFunctions.data();

      for (size_t pixel = 0; pixel < columnsCount; ++pixel)
      {
         const auto color = colorFunction->GetColor(row, defaultColor);

         *rowData++ = color.r;
         *rowData++ = color.g;
         *rowData++ = color.b;

         ++colorFunction;
      }
   }

   element.AvailableColumns = columnsCount;
   element.IsComplete = mLookupHelper->IsComplete;

   element.Bitmap = std::make_unique<wxBitmap>(*mCachedImage);

   return true;
}

