/**********************************************************************

  Audacity: A Digital Audio Editor

  CustomUpdaterValue.cpp

  Dominic Mazzoni
  Michael Papadopoulos split from Ruler.cpp

**********************************************************************/

#include "CustomUpdaterValue.h"

const CustomUpdaterValue& CustomUpdaterValue::Instance()
{
    static CustomUpdaterValue instance;
    return instance;
}

bool CustomUpdaterValue::TickCustom(wxDC& dc, int labelIdx, wxFont font,
                                    // in/out:
                                    TickOutputs outputs,
                                    const RulerStruct& context) const
{
    const double mMin = context.mMin;
    const double mMax = context.mMax;

    const int mLeft = context.mLeft;
    const int mTop = context.mTop;
    const int mRight = context.mRight;
    const int mBottom = context.mBottom;
    const int mLength = context.mLength;
    const int mOrientation = context.mOrientation;

    const RulerStruct::Fonts& mFonts = *context.mpFonts;
    constexpr int spacing = 2;
    const bool mFlip = context.mFlip;
    const TranslatableString mUnits = context.mUnits;
    const bool mLabelEdges = context.mLabelEdges;

    auto TickAtValue
        =[this, &dc, &mFonts, mOrientation, mLabelEdges,
          mMin, mMax, mLength, mRight, mBottom, &context]
          (double value, double* pos) -> bool {
        // Make a tick only if the value is strictly between the bounds
        double min = std::min(mMin, mMax);
        double max = std::max(mMin, mMax);
        if ((value <= min && !mLabelEdges) || value < min) {
            return false;
        }
        if ((value >= max && !mLabelEdges) || value > max) {
            return false;
        }

        int mid = (int)(mLength * ((mMin - value) / (mMin - mMax)) + 0.5);

        const int iMaxPos = (mOrientation == wxHORIZONTAL) ? mRight : mBottom - 5;
        if (mid >= 0 && mid < iMaxPos) {
            *pos = mid;
            return true;
        }
        return false;
    };

    // FIXME: We don't draw a tick if of end of our label arrays
    // But we shouldn't have an array of labels.
    if (labelIdx >= outputs.labels.size()) {
        return false;
    }

    // Get the correct position based on value.
    // Don't draw if the value is out of bounds.
    double pos;
    if (!TickAtValue(outputs.labels[labelIdx].value, &pos)) {
        return false;
    }

    Label lab;
    lab.value = 0.0;
    lab.pos = pos;
    // Custom is flexible with text format
    // We can assume they use the right format, but still append the right units.
    lab.text = outputs.labels[labelIdx].text;
    lab.units = mUnits;

    auto result = MakeTick(
        lab,
        dc, font,
        outputs.bits,
        mLeft, mTop, spacing, mFonts.lead,
        mFlip,
        mOrientation);

    if (!result.second.text) {
        // Always a non-empty optional
        result.second.text = { TranslatableString{} }
    }

    auto& rect = result.first;
    outputs.box.Union(rect);
    outputs.labels[labelIdx] = (result.second);
    return !rect.IsEmpty();
}

CustomUpdaterValue::~CustomUpdaterValue() = default;
