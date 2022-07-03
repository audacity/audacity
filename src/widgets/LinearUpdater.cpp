/**********************************************************************

  Audacity: A Digital Audio Editor

  LinearUpdater.cpp

  Dominic Mazzoni
  Michael Papadopoulos split from Ruler.cpp

**********************************************************************/

#include "LinearUpdater.h"

void LinearUpdater::Update(
   wxDC& dc, const Envelope* envelope, UpdateOutputs& allOutputs) const
{
   TickOutputs majorOutputs{
      allOutputs.majorLabels, allOutputs.bits, allOutputs.box };

   const double mDbMirrorValue = mRuler.mDbMirrorValue;
   const int mLength = mRuler.mLength;
   const Ruler::RulerFormat mFormat = mRuler.mFormat;

   const int mLeft = mRuler.mLeft;
   const int mTop = mRuler.mTop;
   const int mBottom = mRuler.mBottom;
   const int mRight = mRuler.mRight;
   const int mOrientation = mRuler.mOrientation;

   const double mMin = mRuler.mMin;
   const double mMax = mRuler.mMax;
   const double mHiddenMin = mRuler.mHiddenMin;
   const double mHiddenMax = mRuler.mHiddenMax;

   const Ruler::Fonts& mFonts = *mRuler.mpFonts;
   const bool mLabelEdges = mRuler.mLabelEdges;
   const int mLeftOffset = mRuler.mLeftOffset;

   // Use the "hidden" min and max to determine the tick size.
   // That may make a difference with fisheye.
   // Otherwise you may see the tick size for the whole ruler change
   // when the fisheye approaches start or end.
   double UPP = (mHiddenMax - mHiddenMin) / mLength;  // Units per pixel
   TickSizes tickSizes{ UPP, mOrientation, mFormat, false };

   auto TickAtValue =
      [this, &tickSizes, &dc, &majorOutputs, &mFonts, mOrientation,
         mMin, mMax, mLength, mLeftOffset, mRight, mBottom]
   (double value) -> int {
      // Make a tick only if the value is strictly between the bounds
      if (value <= std::min(mMin, mMax))
         return -1;
      if (value >= std::max(mMin, mMax))
         return -1;

      int mid;
      if (zoomInfo) {
         // Tick only at zero
         if (value)
            return -1;
         mid = (int)(zoomInfo->TimeToPosition(0.0, mLeftOffset));
      }
      else
         mid = (int)(mLength * ((mMin - value) / (mMin - mMax)) + 0.5);

      const int iMaxPos = (mOrientation == wxHORIZONTAL) ? mRight : mBottom - 5;
      if (mid >= 0 && mid < iMaxPos)
         Tick(dc, mid, value, tickSizes, mFonts.major, majorOutputs);
      else
         return -1;

      return mid;
   };

   if (mDbMirrorValue) {
      // For dB scale, let the zeroes prevail over the extreme values if
      // not the same, and let midline prevail over all

      // Do the midline
      TickAtValue(-mDbMirrorValue);

      // Do the upper zero
      TickAtValue(0.0);

      // Do the other zero
      TickAtValue(-2 * mDbMirrorValue);
   }

   // Extreme values
   if (mLabelEdges) {
      Tick(dc, 0, mMin, tickSizes, mFonts.major, majorOutputs);
      Tick(dc, mLength, mMax, tickSizes, mFonts.major, majorOutputs);
   }

   if (!mDbMirrorValue) {
      // Zero (if it's strictly in the middle somewhere)
      TickAtValue(0.0);
   }

   double sign = UPP > 0.0 ? 1.0 : -1.0;

   int nDroppedMinorLabels = 0;
   // Major and minor ticks
   for (int jj = 0; jj < 2; ++jj) {
      const double denom = jj == 0 ? tickSizes.mMajor : tickSizes.mMinor;
      auto font = jj == 0 ? mFonts.major : mFonts.minor;
      TickOutputs outputs{
         (jj == 0 ? allOutputs.majorLabels : allOutputs.minorLabels),
         allOutputs.bits, allOutputs.box
      };
      int ii = -1, j = 0;
      double d, warpedD, nextD;

      double prevTime = 0.0, time = 0.0;
      if (zoomInfo) {
         j = zoomInfo->TimeToPosition(mMin);
         prevTime = zoomInfo->PositionToTime(--j);
         time = zoomInfo->PositionToTime(++j);
         d = (prevTime + time) / 2.0;
      }
      else
         d = mMin - UPP / 2;
      if (envelope)
         warpedD = ComputeWarpedLength(*envelope, 0.0, d);
      else
         warpedD = d;
      // using ints doesn't work, as
      // this will overflow and be negative at high zoom.
      double step = floor(sign * warpedD / denom);
      while (ii <= mLength) {
         ii++;
         if (zoomInfo)
         {
            prevTime = time;
            time = zoomInfo->PositionToTime(++j);
            nextD = (prevTime + time) / 2.0;
            // wxASSERT(time >= prevTime);
         }
         else
            nextD = d + UPP;
         if (envelope)
            warpedD += ComputeWarpedLength(*envelope, d, nextD);
         else
            warpedD = nextD;
         d = nextD;

         if (floor(sign * warpedD / denom) > step) {
            step = floor(sign * warpedD / denom);
            bool major = jj == 0;
            tickSizes.useMajor = major;
            bool ticked = Tick(dc, ii, sign * step * denom, tickSizes,
               font, outputs);
            if (!major && !ticked) {
               nDroppedMinorLabels++;
            }
         }
      }
   }

   tickSizes.useMajor = true;

   // If we've dropped minor labels through overcrowding, then don't show
   // any of them.  We're allowed though to drop ones which correspond to the
   // major numbers.
   if (nDroppedMinorLabels >
      (allOutputs.majorLabels.size() + (mLabelEdges ? 2 : 0))) {
      // Old code dropped the labels AND their ticks, like so:
      //    mMinorLabels.clear();
      // Nowadays we just drop the labels.
      for (auto& label : allOutputs.minorLabels)
         label.text = {};
   }

   // Left and Right Edges
   if (mLabelEdges) {
      Tick(dc, 0, mMin, tickSizes, mFonts.major, majorOutputs);
      Tick(dc, mLength, mMax, tickSizes, mFonts.major, majorOutputs);
   }

   BoxAdjust(allOutputs);
}

LinearUpdater::~LinearUpdater() = default;
