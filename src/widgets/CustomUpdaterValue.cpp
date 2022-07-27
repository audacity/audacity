/**********************************************************************

  Audacity: A Digital Audio Editor

  CustomUpdater.cpp

  Dominic Mazzoni

**********************************************************************/


#include "CustomUpdaterValue.h"

bool CustomUpdaterValue::TickCustom(wxDC& dc, int labelIdx, wxFont font,
   // in/out:
   TickOutputs outputs,

   const RulerStruct& context) const
{
   const int mLeft = context.mLeft;
   const int mTop = context.mTop;
   const int mRight = context.mRight;
   const int mBottom = context.mBottom;
   const int mMin = context.mMin;
   const int mMax = context.mMax;
   const int mLength = context.mLength;
   const int mOrientation = context.mOrientation;

   const RulerStruct::Fonts& mFonts = *context.mpFonts;
   const int mSpacing = context.mSpacing;
   const bool mFlip = context.mFlip;
   const TranslatableString mUnits = context.mUnits;

   auto TickAtValue =
      [this, &dc, &mFonts, mOrientation,
      mMin, mMax, mLength, mRight, mBottom, &context]
   (double value) -> int {
      // Make a tick only if the value is strictly between the bounds
      if (value <= std::min(mMin, mMax))
         return -1;
      if (value >= std::max(mMin, mMax))
         return -1;

      int mid = (int)(mLength * ((mMin - value) / (mMin - mMax)) + 0.5);

      const int iMaxPos = (mOrientation == wxHORIZONTAL) ? mRight : mBottom - 5;
      if (mid >= 0 && mid < iMaxPos)
         return mid;
      else
         return -1;
   };

   // FIXME: We don't draw a tick if of end of our label arrays
   // But we shouldn't have an array of labels.
   if (labelIdx >= outputs.labels.size())
      return false;

   Label lab;

   lab.value = 0.0;
   lab.pos = TickAtValue(outputs.labels[labelIdx].value);
   // Custom is flexible with text format
   // We can assume they use the right format, but still append the right units.
   lab.text = outputs.labels[labelIdx].text;
   lab.units = mUnits;

   const auto result = MakeTick(
      lab,
      dc, font,
      outputs.bits,
      mLeft, mTop, mSpacing, mFonts.lead,
      mFlip,
      mOrientation);

   auto& rect = result.first;
   outputs.box.Union(rect);
   outputs.labels[labelIdx] = (result.second);
   return !rect.IsEmpty();
}

CustomUpdaterValue::~CustomUpdaterValue() = default;
