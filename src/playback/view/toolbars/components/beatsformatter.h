/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "timecodeformatter.h"

namespace au::playback {
class BeatsFormatter : public TimecodeFormatter
{
public:
    BeatsFormatter(const QString& formatStr, int fracPart);

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
    int m_fieldValueOffset = 1;
    int m_fracPart = 0;
};
}
