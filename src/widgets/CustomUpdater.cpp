/**********************************************************************

  Audacity: A Digital Audio Editor

  CustomUpdater.cpp

  Dominic Mazzoni
  Michael Papadopoulos split from Ruler.cpp

**********************************************************************/


#include "CustomUpdater.h"

void CustomUpdater::Update(
   wxDC& dc, const Envelope* envelope, UpdateOutputs& allOutputs) const
{
   const int mLength = mRuler.mLength;
   const Ruler::Fonts& mFonts = *mRuler.mpFonts;

   TickOutputs majorOutputs{
      allOutputs.majorLabels, allOutputs.bits, allOutputs.box };

   // SET PARAMETER IN MCUSTOM CASE
   // Works only with major labels

   int numLabel = allOutputs.majorLabels.size();

   for (int i = 0; (i < numLabel) && (i <= mLength); ++i)
      TickCustom(dc, i, mFonts.major, majorOutputs);

   BoxAdjust(allOutputs);
}

CustomUpdater::~CustomUpdater() = default;
