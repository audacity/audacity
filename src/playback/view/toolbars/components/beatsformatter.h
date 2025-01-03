/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <array>

#include "timecodeformatter.h"

enum class BeatsFormatterMode {
    TimePoint,  // 1-base indexing 00:00:00s -> 1 bar 1 beat 1
    Duration    // 0-base indexing 00:00:00s -> 0 bar 0 beat 0
};

namespace au::playback {
class BeatsFormatter : public TimecodeFormatter
{
public:
    BeatsFormatter(const QString& formatStr, int fracPart, BeatsFormatterMode mode = BeatsFormatterMode::TimePoint);

    void init() override;

    ConversionResult valueToString(double value, bool nearest) const override;

    std::optional<double> stringToValue(const QString& value) const override;

    double singleStep(double value, int digitIndex, bool upwards) override;

private:
    void updateResultString(ConversionResult& result) const;

    bool checkField(size_t fieldIndex, int value) const;
    bool checkFracField(int newLts) const;
    void updateFields(size_t barsDigits);

    std::array<double, 3> m_fieldLengths;
    int m_fracPart = 0;
    BeatsFormatterMode m_mode;
};
}
