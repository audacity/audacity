/**********************************************************************

  Audacity: A Digital Audio Editor

  GeneratedUpdater.cpp

  Dominic Mazzoni
  Michael Papadopoulos split from Ruler.cpp

**********************************************************************/

#include "GeneratedUpdater.h"

bool GeneratedUpdater::Tick(wxDC& dc,
                            int pos, double d, const TickSizes& tickSizes, wxFont font,
                            // in/out:
                            TickOutputs outputs,
                            const RulerStruct& context) const
{
    const double mDbMirrorValue = context.mDbMirrorValue;
    const int mLength = context.mLength;

    const int mLeft = context.mLeft;
    const int mTop = context.mTop;
    const int mBottom = context.mBottom;
    const int mRight = context.mRight;
    const int mOrientation = context.mOrientation;

    const RulerStruct::Fonts& mFonts = *context.mpFonts;
    const TranslatableString mUnits = context.mUnits;
    const int spacing = (mOrientation == wxHORIZONTAL) ? 6 : 2;
    const bool mFlip = context.mFlip;

    // Bug 521.  dB view for waveforms needs a 2-sided scale.
    if ((mDbMirrorValue > 1.0) && (-d > mDbMirrorValue)) {
        d = -2 * mDbMirrorValue - d;
    }

    // FIXME: We don't draw a tick if off end of our label arrays
    // But we shouldn't have an array of labels.
    if (outputs.labels.size() >= mLength) {
        return false;
    }

    Label lab;
    lab.value = d;
    lab.pos = pos;
    lab.text = tickSizes.LabelString(d, context.mpRulerFormat);
    lab.units = mUnits;

    const auto result = RulerUpdater::MakeTick(
        lab,
        dc, font,
        outputs.bits,
        mLeft, mTop, spacing, mFonts.lead,
        mFlip,
        mOrientation);

    auto& rect = result.first;
    outputs.box.Union(rect);
    outputs.labels.emplace_back(result.second);
    return !rect.IsEmpty();
}

GeneratedUpdater::~GeneratedUpdater() = default;
