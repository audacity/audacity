/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QString>

#include "types.h"

namespace au::playback {
struct FieldConfig final
{
    bool frac = false; // is it a fractional field
    int base = 0;  // divide by this (multiply, after decimal point)
    // Code in the parser converts range to `long`
    long range = 0; // then take modulo this
};

class NumericConverterFormatter
{
public:

    NumericConverterFormatter(NumericType type, const QString& formatStr);

    struct ConversionResult
    {
        QString valueString;
        std::vector<QString> fieldValueStrings;
    };

    void parseFormatString();

    ConversionResult valueToString(double value, bool nearest) const;

    std::optional<double> stringToValue(const QString& value) const;

    double singleStep(double value, int digitIndex, bool upwards);

    const QString& prefix() const;
    const NumericFields& fields() const;
    const DigitInfos& digitInfos() const;

    bool isTimeRelatedFormat() const;

    void setSampleRate(double sampleRate);

protected:
    QString m_prefix;

    NumericFields m_fields;
    std::vector<FieldConfig> m_fieldConfigs;
    DigitInfos m_digits;

    double m_scalingFactor = 0.0;

    double m_sampleRate = 1.0;
    bool m_scalingFactorIsSamples = false;

    bool m_ntscDrop = false;

    NumericType m_type = NumericType::Time;
    QString m_format;
};
}
