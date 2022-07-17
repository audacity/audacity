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

   // SET PARAMETER IN MCUSTOM CASE
   // Works only with major labels

   int numLabel = allOutputs.majorLabels.size();

   for (int i = 0; (i < numLabel) && (i <= mLength); ++i)
      TickCustom(dc, i, mFonts.major, majorOutputs, context);

   BoxAdjust(allOutputs, context);
}

CustomUpdater::~CustomUpdater() = default;
