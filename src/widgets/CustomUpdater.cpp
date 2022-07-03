/**********************************************************************

  Audacity: A Digital Audio Editor

  CustomUpdater.cpp

  Dominic Mazzoni
  Michael Papadopoulos split from Ruler.cpp

**********************************************************************/


#include "CustomUpdater.h"

void CustomUpdater::Update(
   wxDC& dc, const Envelope* envelope,
   UpdateOutputs& allOutputs, const RulerStruct& context) const
{
   const int mLength = context.mLength;
   const RulerStruct::Fonts& mFonts = *context.mpFonts;

   TickOutputs majorOutputs{
      allOutputs.majorLabels, allOutputs.bits, allOutputs.box };

   // SET PARAMETER IN MCUSTOM CASE
   // Works only with major labels

   int numLabel = allOutputs.majorLabels.size();

   for (int i = 0; (i < numLabel) && (i <= mLength); ++i)
      TickCustom(dc, i, mFonts.major, majorOutputs, context);

   BoxAdjust(allOutputs, context);
}

bool CustomUpdater::TickCustom(wxDC& dc, int labelIdx, wxFont font,
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

   // FIXME: We don't draw a tick if of end of our label arrays
   // But we shouldn't have an array of labels.
   if (labelIdx >= outputs.labels.size())
      return false;

   //This should only used in the mCustom case

   Label lab;
   lab.value = 0.0;

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

CustomUpdater::~CustomUpdater() = default;
