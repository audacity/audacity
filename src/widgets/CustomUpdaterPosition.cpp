/**********************************************************************

  Audacity: A Digital Audio Editor

  CustomUpdaterPosition.cpp

  Dominic Mazzoni

**********************************************************************/


#include "CustomUpdaterPosition.h"

bool CustomUpdaterPosition::TickCustom(wxDC& dc, int labelIdx, wxFont font,
   // in/out:
   TickOutputs outputs,
   const RulerStruct& context) const
{
   const int mLeft = context.mLeft;
   const int mTop = context.mTop;
   const int mOrientation = context.mOrientation;

   const RulerStruct::Fonts& mFonts = *context.mpFonts;
   const int mSpacing = context.mSpacing;
   const bool mFlip = context.mFlip;
   const TranslatableString mUnits = context.mUnits;

   // FIXME: We don't draw a tick if of end of our label arrays
   // But we shouldn't have an array of labels.
   if (labelIdx >= outputs.labels.size())
      return false;

   Label lab;

   lab.value = 0.0;
   lab.pos = outputs.labels[labelIdx].pos;
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

CustomUpdaterPosition::~CustomUpdaterPosition() = default;
