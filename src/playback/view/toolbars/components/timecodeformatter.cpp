/*
* Audacity: A Digital Audio Editor
*/
#include "timecodeformatter.h"

using namespace au::playback;

TimecodeFormatter::TimecodeFormatter(const QString& formatStr)
    : m_format(formatStr)
{
}

void TimecodeFormatter::init()
{
}

TimecodeFormatter::ConversionResult TimecodeFormatter::valueToString(double, bool) const
{
    return TimecodeFormatter::ConversionResult();
}

std::optional<double> TimecodeFormatter::stringToValue(const QString&) const
{
    return {};
}

double TimecodeFormatter::singleStep(double value, int, bool)
{
    return value;
}

double TimecodeFormatter::singleStep(const QString& valueStr, int digitIndex, bool upwards)
{
    return singleStep(stringToValue(valueStr).value(), digitIndex, upwards);
}

void TimecodeFormatter::setSampleRate(double sampleRate)
{
    m_sampleRate = sampleRate;
}

void TimecodeFormatter::setTempo(double tempo)
{
    m_tempo = tempo;
}

void TimecodeFormatter::setUpperTimeSignature(int timeSig)
{
    m_upperTimeSignature = timeSig;
}

void TimecodeFormatter::setLowerTimeSignature(int timeSig)
{
    m_lowerTimeSignature = timeSig;
}
