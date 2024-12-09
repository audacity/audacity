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

class TimecodeFormatter
{
public:
    TimecodeFormatter(const QString& formatStr);
    virtual ~TimecodeFormatter() = default;

    struct ConversionResult
    {
        QString valueString;
        std::vector<QString> fieldValueStrings;
    };

    virtual void init();

    virtual ConversionResult valueToString(double value, bool nearest) const;
    virtual std::optional<double> stringToValue(const QString& value) const;

    virtual double singleStep(double value, int digitIndex, bool upwards);
    virtual double singleStep(const QString& valueStr, int digitIndex, bool upwards);

    void setSampleRate(double sampleRate);

    void setTempo(double tempo);
    void setUpperTimeSignature(int timeSig);
    void setLowerTimeSignature(int timeSig);

protected:
    QString m_prefix;

    NumericFields m_fields;
    std::vector<FieldConfig> m_fieldConfigs;
    DigitInfos m_digits;

    double m_sampleRate = 1.0;

    double m_tempo = 0;
    int m_upperTimeSignature = -1;
    int m_lowerTimeSignature = -1;

    QString m_format;
};
}
