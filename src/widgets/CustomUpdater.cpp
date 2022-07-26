/**********************************************************************

  Audacity: A Digital Audio Editor

  CustomUpdater.cpp

  Dominic Mazzoni

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

CustomUpdater::~CustomUpdater() = default;
