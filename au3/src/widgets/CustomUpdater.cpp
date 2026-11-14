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

    allOutputs.majorLabels = mMajorLabels;
    allOutputs.minorLabels = mMinorLabels;
    allOutputs.minorMinorLabels = mMinorMinorLabels;

    for (int ii = 0; ii < 3; ii++) {
        Labels& labs = (ii == 0) ? allOutputs.majorLabels
                       : (ii == 1) ? allOutputs.minorLabels : allOutputs.minorMinorLabels;

        wxFont font = (ii == 0) ? mFonts.major
                      : (ii == 1) ? mFonts.minor : mFonts.minorMinor;

        TickOutputs outputs{ labs, allOutputs.bits, allOutputs.box };
        int numLabel = labs.size();

        for (int i = 0; (i < numLabel) && (i <= mLength); ++i) {
            TickCustom(dc, i, font, outputs, context);
        }
    }

    BoxAdjust(allOutputs, context);
}

CustomUpdater::~CustomUpdater() = default;
