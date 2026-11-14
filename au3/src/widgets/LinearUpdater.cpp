/**********************************************************************

  Audacity: A Digital Audio Editor

  LinearUpdater.cpp

  Dominic Mazzoni
  Michael Papadopoulos split from Ruler.cpp

**********************************************************************/

#include "LinearUpdater.h"
#include "BeatsFormat.h"

const LinearUpdater& LinearUpdater::Instance()
{
    static LinearUpdater instance;
    return instance;
}

void LinearUpdater::Update(
    wxDC& dc, const Envelope* envelope,
    UpdateOutputs& allOutputs, const RulerStruct& context) const
{
    TickOutputs majorOutputs{
        allOutputs.majorLabels, allOutputs.bits, allOutputs.box };

    const double mDbMirrorValue = context.mDbMirrorValue;
    const int mLength = context.mLength;

    const int mLeft = context.mLeft;
    const int mTop = context.mTop;
    const int mBottom = context.mBottom;
    const int mRight = context.mRight;
    const int mOrientation = context.mOrientation;

    const double mMin = context.mMin;
    const double mMax = context.mMax;
    const double mHiddenMin = context.mHiddenMin;
    const double mHiddenMax = context.mHiddenMax;

    const RulerStruct::Fonts& mFonts = *context.mpFonts;
    const bool mLabelEdges = context.mLabelEdges;

    // Use the "hidden" min and max to determine the tick size.
    // That may make a difference with fisheye.
    // Otherwise you may see the tick size for the whole ruler change
    // when the fisheye approaches start or end.
    double UPP = (mHiddenMax - mHiddenMin) / mLength; // Units per pixel
    TickSizes tickSizes{ UPP, mOrientation, context.mpRulerFormat, false };

    auto TickAtValue
        =[this, &tickSizes, &dc, &majorOutputs, &mFonts, mOrientation,
          mMin, mMax, mLength, mRight, mBottom, &context]
          (double value) -> int {
        // Make a tick only if the value is strictly between the bounds
        if (value < std::min(mMin, mMax)) {
            return -1;
        }
        if (value > std::max(mMin, mMax)) {
            return -1;
        }

        int mid;
        if (mpZoomInfo) {
            // Tick only at zero
            if (value) {
                return -1;
            }
            mid = (int)(mpZoomInfo->TimeToPosition(0.0, mLeftOffset));
        } else {
            mid = (int)(mLength * ((mMin - value) / (mMin - mMax)) + 0.5);
        }

        const int iMaxPos = (mOrientation == wxHORIZONTAL) ? mRight : mBottom - 5;
        if (mid >= 0 && mid < iMaxPos) {
            Tick(dc, mid, value, tickSizes, mFonts.major, majorOutputs, context);
        } else {
            return -1;
        }

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

    if (!mDbMirrorValue) {
        // Zero (if it's strictly in the middle somewhere)
        TickAtValue(0.0);
    }

    double sign = UPP > 0.0 ? 1.0 : -1.0;

    int nDroppedMinorLabels = 0;
    // Major and minor ticks
    for (int jj = 0; jj < 3; ++jj) {
        const double denom = jj == 0 ? tickSizes.mMajor
                             : jj == 1 ? tickSizes.mMinor : tickSizes.mMinorMinor;
        tickSizes.tickType = jj == 0 ? RulerFormat::t_major
                             : jj == 1 ? RulerFormat::t_minor : RulerFormat::t_minorMinor;
        if (denom == 0) {
            continue;
        }
        auto font = jj == 0 ? mFonts.major
                    : jj == 1 ? mFonts.minor : mFonts.minorMinor;
        TickOutputs outputs{
            (jj == 0 ? allOutputs.majorLabels
             : jj == 1 ? allOutputs.minorLabels : allOutputs.minorMinorLabels),
            allOutputs.bits, allOutputs.box
        };
        int ii = -1, j = 0;
        double d, warpedD, nextD;

        double prevTime = 0.0, time = 0.0;
        if (mpZoomInfo) {
            j = mpZoomInfo->TimeToPosition(mMin);
            prevTime = mpZoomInfo->PositionToTime(--j);
            time = mpZoomInfo->PositionToTime(++j);
            d = (prevTime + time) / 2.0;
        } else {
            d = mMin - UPP / 2;
        }
        if (envelope) {
            warpedD = ComputeWarpedLength(*envelope, 0.0, d);
        } else {
            warpedD = d;
        }
        // using ints doesn't work, as
        // this will overflow and be negative at high zoom.
        double step = floor(sign * warpedD / denom);
        while (ii <= mLength) {
            ii++;
            if (mpZoomInfo) {
                prevTime = time;
                time = mpZoomInfo->PositionToTime(++j);
                nextD = (prevTime + time) / 2.0;
                // wxASSERT(time >= prevTime);
            } else {
                nextD = d + UPP;
            }
            if (envelope) {
                warpedD += ComputeWarpedLength(*envelope, d, nextD);
            } else {
                warpedD = nextD;
            }
            d = nextD;

            if (floor(sign * warpedD / denom) > step) {
                step = floor(sign * warpedD / denom);
                bool ticked = Tick(dc, ii, sign * step * denom, tickSizes,
                                   font, outputs, context);
                bool major = jj == 0;
                if (!major && !ticked) {
                    nDroppedMinorLabels++;
                }
            }
        }
    }

    tickSizes.tickType = RulerFormat::t_major;

    // If we've dropped minor labels through overcrowding, then don't show
    // any of them.  We're allowed though to drop ones which correspond to the
    // major numbers.
    // TODO: Doesn't play nicely with BeatsFormat, possibly because of dropping values less than 0?
    // Investigate and come up with a better solution than specifically ignoring BeatsFormat

    if (nDroppedMinorLabels
        > (allOutputs.majorLabels.size() + (mLabelEdges ? 2 : 0))
        && !dynamic_cast<const BeatsFormat*>(context.mpRulerFormat)) {
        // Old code dropped the labels AND their ticks, like so:
        //    mMinorLabels.clear();
        // Nowadays we just drop the labels.
        for (auto& label : allOutputs.minorLabels) {
            label.text = {};
            label.units = {};
        }
        for (auto& label : allOutputs.minorMinorLabels) {
            label.text = {};
            label.units = {};
        }
    }

    // Left and Right Edges
    if (mLabelEdges) {
        Tick(dc, 0, mMin, tickSizes, mFonts.major, majorOutputs, context);
        Tick(dc, mLength, mMax, tickSizes, mFonts.major, majorOutputs, context);
    }

    BoxAdjust(allOutputs, context);
}

LinearUpdater::~LinearUpdater() = default;
