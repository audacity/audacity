/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QString>

#include "types.h"
#include "timecodeformatter.h"

namespace au::playback {
class NumericFormatter : public TimecodeFormatter
{
public:
    NumericFormatter(const QString& formatStr);

    void init() override;

    ConversionResult valueToString(double value, bool nearest) const override;

    std::optional<double> stringToValue(const QString& value) const override;

    double singleStep(double value, int digitIndex, bool upwards) override;

protected:
    double m_scalingFactor = 0.0;
    bool m_scalingFactorIsSamples = false;

    bool m_ntscDrop = false;

    NumericType m_type = NumericType::Time;
};
}
