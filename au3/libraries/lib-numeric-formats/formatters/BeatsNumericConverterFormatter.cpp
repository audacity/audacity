/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

 Audacity: A Digital Audio Editor

 @file BeatsNumericConverterFormatter.cpp

 Dmitry Vedenko

 **********************************************************************/
#include "BeatsNumericConverterFormatter.h"

#include <algorithm>
#include <array>
#include <cmath>

#include "NumericConverterRegistry.h"
#include "NumericConverterFormatterContext.h"

#include "SampleCount.h"

#include "Project.h"
#include "ProjectTimeSignature.h"

namespace {
// This function will return 10^pow
// No overflow checks are performed, it is assumed that 10^pow
// does not overflow
constexpr size_t Get10Pow(size_t pow)
{
    return pow > 0 ? 10 * Get10Pow(pow - 1) : 1;
}

/* i18n-hint: The music theory "bar" */
const auto BarString = XO("bar");
/* i18n-hint: The music theory "beat" */
const auto BeatString = XO("beat");

class BeatsFormatter final : public NumericConverterFormatter, public PrefsListener
{
public:
    static constexpr std::array<size_t, 3> MIN_DIGITS { 3, 2, 2 };
    static constexpr std::array<size_t, 3> UPPER_BOUNDS {
        Get10Pow(MIN_DIGITS[0] - 1) + 1, Get10Pow(MIN_DIGITS[1] - 1) + 1,
        Get10Pow(MIN_DIGITS[2] - 1) + 1
    };

    BeatsFormatter(const FormatterContext& context, int fracPart, bool timeFormat)
        : mContext{context}
        , mFracPart{fracPart}
        , mFieldValueOffset{timeFormat ? 1 : 0}
    {
        auto project = mContext.GetProject();

        if (!project) {
            return;
        }

        mBarString = BarString.Translation();
        mBeatString = BeatString.Translation();

        UpdateFormat(*project);

        // Subscribing requires non-const reference
        mTimeSignatureChangedSubscription
            =const_cast<ProjectTimeSignature&>(ProjectTimeSignature::Get(*project))
              .Subscribe(
                  [this](const auto&)
        {
            // Receiving this message means that project is
            // alive and well
            UpdateFormat(*mContext.GetProject());
            Publish({});
        });
    }

    //! Check that field exists and has enough digits to fit the value
    bool CheckField(size_t fieldIndex, int value) const noexcept
    {
        if (fieldIndex >= mFields.size()) {
            return false;
        }

        const auto digitsCount = mFields[fieldIndex].digits;

        // Format always allows at least two digits
        const auto lowerRange
            =digitsCount > MIN_DIGITS[fieldIndex] ? Get10Pow(digitsCount - 1) : 0;

        const auto upperRange = Get10Pow(digitsCount);

        return value >= int(lowerRange) && value < int(upperRange);
    }

    bool CheckFracField(int newLts) const noexcept
    {
        if (mFracPart > newLts) {
            return CheckField(2, mFracPart / mLowerTimeSignature);
        } else {
            return mFields.size() == 2;
        }
    }

    void UpdateFields(size_t barsDigits)
    {
        mFields.clear();
        mDigits.clear();

        // Range is assumed to allow 999 bars.
        auto& barsField
            =mFields.emplace_back(NumericField::WithDigits(barsDigits));

        barsField.label = L" " + mBarString + L" ";

        // Beats format is 1 based. For the time point "0" the expected output is
        // "1 bar 1 beat [1]" For this reason we use (uts + 1) as the "range". On
        // top of that, we want at least two digits to be shown. NumericField
        // accepts range as in [0, range), so add 1.

        auto& beatsField = mFields.emplace_back(NumericField::ForRange(
                                                    std::max<size_t>(UPPER_BOUNDS[1], mUpperTimeSignature + 1)));

        beatsField.label = L" " + mBeatString;

        const auto hasFracPart = mFracPart > mLowerTimeSignature;

        if (hasFracPart) {
            beatsField.label += L" ";
            // See the reasoning above about the range
            auto& fracField = mFields.emplace_back(NumericField::ForRange(
                                                       std::max(11, mFracPart / mLowerTimeSignature + 1)));
        }

        // Fill the aux mDigits structure
        size_t pos = 0;
        for (size_t i = 0; i < mFields.size(); i++) {
            mFields[i].pos = pos;

            for (size_t j = 0; j < mFields[i].digits; j++) {
                mDigits.push_back(DigitInfo { i, j, pos });
                pos++;
            }

            pos += mFields[i].label.length();
        }
    }

    void UpdateFormat(const AudacityProject& project)
    {
        auto& timeSignature = ProjectTimeSignature::Get(project);

        const double newTempo = timeSignature.GetTempo();
        const int newUts = timeSignature.GetUpperTimeSignature();
        const int newLts = timeSignature.GetLowerTimeSignature();

        if (newTempo == mTempo && newUts == mUpperTimeSignature && newLts == mLowerTimeSignature) {
            return;
        }

        const bool formatOk = CheckField(1, newUts) && CheckFracField(newLts);

        mTempo = newTempo;
        mUpperTimeSignature = newUts;
        mLowerTimeSignature = newLts;

        // 1/4 = BPM is used for now
        const auto quarterLength = 60.0 / mTempo;
        const auto beatLength = quarterLength * 4.0 / mLowerTimeSignature;
        const auto barLength = mUpperTimeSignature * beatLength;

        mFieldLengths[0] = barLength;
        mFieldLengths[1] = beatLength;

        const auto hasFracPart = mFracPart > mLowerTimeSignature;

        if (hasFracPart) {
            const auto fracLength = beatLength * mLowerTimeSignature / mFracPart;
            mFieldLengths[2] = fracLength;
        }

        if (formatOk) {
            return;
        }

        UpdateFields(MIN_DIGITS[0]);
    }

    void UpdateFormatForValue(double value, bool canShrink) override
    {
        // Beats formatter does not support negative values
        value = std::max(0.0, value);

        // ForRange has a preserved weird behavior
        const auto barsCount
            =// Range is not inclusive
              1
              +// Bars can start from 1
              mFieldValueOffset
              + static_cast<int>(std::floor(value / mFieldLengths[0]));

        const auto barsField = NumericField::ForRange(
            barsCount, true, MIN_DIGITS[0]);

        const auto oldDigits = mFields[0].digits;

        const bool updateNeeded = canShrink ? oldDigits != barsField.digits
                                  : oldDigits < barsField.digits;

        if (!updateNeeded) {
            return;
        }

        UpdateFields(barsField.digits);
        Publish({ value, oldDigits > mFields[0].digits });
    }

    void UpdateResultString(ConversionResult& result) const
    {
        for (size_t fieldIndex = 0; fieldIndex < mFields.size(); ++fieldIndex) {
            result.valueString
                +=result.fieldValueStrings[fieldIndex] + mFields[fieldIndex].label;
        }
    }

    ConversionResult ValueToString(double value, bool) const override
    {
        ConversionResult result;
        result.fieldValueStrings.resize(mFields.size());

        if (value < 0) {
            for (size_t fieldIndex = 0; fieldIndex < mFields.size(); ++fieldIndex) {
                const auto digitsCount = mFields[fieldIndex].digits;
                auto& fieldValue = result.fieldValueStrings[fieldIndex];
                for (int digitIndex = 0; digitIndex < digitsCount; ++digitIndex) {
                    fieldValue += L"-";
                }
            }

            UpdateResultString(result);

            return result;
        }

        // Calculate the epsilon only once, so the total loss of precision is addressed.
        // This is a "multiplicative" epsilon, so there is no need to calculate 1 + eps every time.
        const auto eps
            =1.0 + std::max(1.0, value) * std::numeric_limits<double>::epsilon();

        for (size_t fieldIndex = 0; fieldIndex < mFields.size(); ++fieldIndex) {
            const auto fieldLength = mFieldLengths[fieldIndex];
            const auto fieldValue = std::max(
                0, static_cast<int>(std::floor(value * eps / fieldLength)));

            result.fieldValueStrings[fieldIndex] = wxString::Format(
                mFields[fieldIndex].formatStr, fieldValue + mFieldValueOffset);

            value = value - fieldValue * fieldLength;
        }

        UpdateResultString(result);
        return result;
    }

    std::optional<double> StringToValue(const wxString& valueString) const override
    {
        if (
            mFields.size() > 0
            && valueString.Mid(mFields[0].pos, 1) == wxChar('-')) {
            return std::nullopt;
        }

        double t = 0.0;
        size_t lastIndex = 0;

        for (size_t i = 0; i < mFields.size(); i++) {
            const auto& field = mFields[i];

            const size_t labelIndex = field.label.empty()
                                      ? wxString::npos
                                      : valueString.find(field.label, lastIndex);

            long val;

            const auto fieldStringValue = valueString.Mid(
                lastIndex,
                labelIndex == wxString::npos ? labelIndex : labelIndex - lastIndex);

            if (!fieldStringValue.ToLong(&val)) {
                return std::nullopt;
            }

            t += (val - mFieldValueOffset) * mFieldLengths[i];

            lastIndex = labelIndex + field.label.Length();
        }

        return t;
    }

    double SingleStep(double value, int digitIndex, bool upwards) const override
    {
        if (digitIndex < 0 || size_t(digitIndex) >= mDigits.size()) {
            return value;
        }

        const auto& digit = mDigits[digitIndex];
        const auto& fieldIndex = digit.field;
        const auto& field = mFields[fieldIndex];

        const auto stepSize = mFieldLengths[fieldIndex]
                              * std::pow(10, field.digits - digit.index - 1);

        return upwards ? value + stepSize : value - stepSize;
    }

    void UpdatePrefs() override
    {
        auto project = mContext.GetProject();

        if (!project) {
            return;
        }

        auto barString = BarString.Translation();
        auto beatString = BeatString.Translation();

        if (barString == mBarString && beatString == mBeatString) {
            return;
        }

        mBarString = barString;
        mBeatString = beatString;

        UpdateFormat(*project);
    }

private:
    const FormatterContext mContext;

    Observer::Subscription mTimeSignatureChangedSubscription;

    double mTempo { 0.0 };

    int mUpperTimeSignature { 0 };
    int mLowerTimeSignature { 0 };

    const int mFracPart;

    const int mFieldValueOffset;

    std::array<double, 3> mFieldLengths {};

    wxString mBarString;
    wxString mBeatString;
};

class BeatsNumericConverterFormatterFactory final : public NumericConverterFormatterFactory
{
public:
    BeatsNumericConverterFormatterFactory (int fracPart, bool timeFormat)
        : mFracPart{fracPart}
        , mTimeFormat{timeFormat}
    {
    }

    std::unique_ptr<NumericConverterFormatter>
    Create(const FormatterContext& context) const override
    {
        if (!IsAcceptableInContext(context)) {
            return {}
        }

        return std::make_unique<BeatsFormatter>(context, mFracPart, mTimeFormat);
    }

    bool IsAcceptableInContext(const FormatterContext& context) const override
    {
        return context.HasProject();
    }

private:
    const int mFracPart;
    const bool mTimeFormat;
};

auto BuildBeatsGroup(bool timeFormat)
{
    return NumericConverterFormatterGroup(
        timeFormat ? "beatsTime" : "beatsDuration",
        timeFormat ? NumericConverterType_TIME() : NumericConverterType_DURATION(),
        NumericConverterFormatterItem(
            /* i18n-hint: "bar" and "beat" are musical notation elements. */
            "beats", XO("bar:beat"),
            std::make_unique<BeatsNumericConverterFormatterFactory>(0, timeFormat)),
        NumericConverterFormatterItem(
            /* i18n-hint: "bar" and "beat" are musical notation elements. "tick"
               corresponds to a 16th note.  */
            "beats16", XO("bar:beat:tick"),
            std::make_unique<BeatsNumericConverterFormatterFactory>(16, timeFormat)));
}

NumericConverterItemRegistrator beatsTime {
    BuildBeatsGroup(true),
    Registry::Placement { "parsed", { Registry::OrderingHint::After, L"parsedTime" } }
};

NumericConverterItemRegistrator beatsDuration {
    BuildBeatsGroup(false),
    Registry::Placement { "parsed", { Registry::OrderingHint::After, L"parsedDuration" } }
};
} // namespace

std::unique_ptr<NumericConverterFormatter> CreateBeatsNumericConverterFormatter(
    const FormatterContext& context, int fracPart /*= 0*/,
    bool timeFormat /*= true*/)
{
    return std::make_unique<BeatsFormatter>(context, fracPart, timeFormat);
}
