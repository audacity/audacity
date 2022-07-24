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
   int numMajorLabel = allOutputs.majorLabels.size();
   for (int i = 0; (i < numMajorLabel) && (i <= mLength); ++i)
      TickCustom(dc, i, mFonts.major, majorOutputs, context);


   TickOutputs minorOutputs{
      allOutputs.minorLabels, allOutputs.bits, allOutputs.box };
   int numMinorLabel = allOutputs.minorLabels.size();
   for (int i = 0; (i < numMinorLabel) && (i <= mLength); ++i)
      TickCustom(dc, i, mFonts.minor, minorOutputs, context);

   TickOutputs minorMinorOutputs{
      allOutputs.minorMinorLabels, allOutputs.bits, allOutputs.box };
   int numMinorMinorLabel = allOutputs.minorMinorLabels.size();
   for (int i = 0; (i < numMinorMinorLabel) && (i <= mLength); ++i)
      TickCustom(dc, i, mFonts.minorMinor, minorMinorOutputs, context);

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

CustomUpdater::~CustomUpdater() = default;
