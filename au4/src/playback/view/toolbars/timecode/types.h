/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QString>

#include "containers.h"

namespace au::playback {
enum class NumericType
{
    Time,
    Duration,
    Frequency,
    Bandwidth
};

static size_t calculateDigits(size_t rangeEnd)
{
    if (rangeEnd == 0) {
        return 0;
    }

    --rangeEnd;

    size_t digitsCount = 0;

    while (rangeEnd > 0) {
        rangeEnd /= 10;
        ++digitsCount;
    }

    return digitsCount;
}

struct NumericField
{
private:
    NumericField(size_t digits, bool zeropad)
        : digits(digits)
    {
        if (zeropad && digits > 1) {
            formatStr = "%0" + QString("%0d").arg(digits); // ex. "%03d" if digits is 3
        } else {
            formatStr = "%d";
        }
    }

public:
    static NumericField range(size_t range, bool zeropad = true, size_t minDigits = 0)
    {
        return NumericField(range > 1 ? std::max(minDigits, calculateDigits(range)) : 5, zeropad);
    }

    static NumericField withDigits(size_t digits, bool zeropad = true)
    {
        return NumericField(digits, zeropad);
    }

    NumericField(const NumericField&) = default;
    NumericField& operator=(const NumericField&) = default;

    size_t digits = 0;

    QString label;
    QString formatStr;

    size_t pos = muse::nidx; // Index of this field in the ValueString
};

using NumericFields = std::vector<NumericField>;

struct DigitInfo
{
    size_t field = 0; // Which field
    size_t index = 0; // Index of this digit within the field
    size_t pos = 0;   // Position in the ValueString
};

using DigitInfos = std::vector<DigitInfo>;
}
