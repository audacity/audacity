/**********************************************************************

  Audacity: A Digital Audio Editor

**********************************************************************/

#include "beatsmeasuresformat.h"

using namespace au::projectscene;

IntervalInfo BeatsMeasuresFormat::intervalInfo(TimelineContext* context)
{
    IntervalInfo timeInterval;
    double zoom = context->zoom();
    int BPM = context->BPM();
    // Seconds Per Beat
    double SPB = static_cast<double>(60) / BPM;
    double timeSigUpper = context->timeSigUpper();
    double timeSigLower = static_cast<double>(context->timeSigLower());

    if (zoom > (28 / ((4 / timeSigLower) * SPB / 32))) { // 1/128 ticks
        timeInterval.minor = 60 / (BPM * (timeSigLower * 2));
        timeInterval.major = (60 * timeSigUpper) / (BPM * (timeSigLower / 4));
        timeInterval.minorMinor = 60 / (BPM * (timeSigLower * 8));

        return timeInterval;
    }
    if (zoom > (32 / ((4 / timeSigLower) * SPB / 16))) { // 1/64 ticks
        timeInterval.minor = 60 / (BPM * (timeSigLower));
        timeInterval.major = (60 * timeSigUpper) / (BPM * (timeSigLower / 4));
        timeInterval.minorMinor = 60 / (BPM * (timeSigLower * 4));
        return timeInterval;
    }
    if (zoom > (32 / ((4 / timeSigLower) * SPB / 8))) { // 1/32 beat ticks
        timeInterval.minor = 60 / (BPM * (timeSigLower / 2));
        timeInterval.major = (60 * timeSigUpper) / (BPM * (timeSigLower / 4));
        timeInterval.minorMinor = 60 / (BPM * (timeSigLower * 2));
        return timeInterval;
    }
    if (zoom > (24 / ((4 / timeSigLower) * SPB / 4))) { // 1/16 beat ticks
        timeInterval.minor = 60 / (BPM * (timeSigLower / 4));
        timeInterval.major = (60 * timeSigUpper) / (BPM * (timeSigLower / 4));
        timeInterval.minorMinor = 60 / (BPM * (timeSigLower));
        return timeInterval;
    }
    if (zoom > (24 / ((4 / timeSigLower) * SPB / 2))) {  // 1/8 beat ticks
        timeInterval.minor = 60 / (BPM * (timeSigLower / 4));
        timeInterval.major = (60 * timeSigUpper) / (BPM * (timeSigLower / 4));
        timeInterval.minorMinor = 60 / (BPM * (timeSigLower / 2));
        return timeInterval;
    }
    if (zoom > (12 / ((4 / timeSigLower) * SPB))) { // 1/4 beat ticks
        timeInterval.minor = std::numeric_limits<int>::max(); // do not draw minor ticks
        timeInterval.major = (60 * timeSigUpper) / (BPM * (timeSigLower / 4));
        timeInterval.minorMinor = 60 / (BPM * (timeSigLower / 4));

        return timeInterval;
    }
    if (zoom > (12 / ((4 / timeSigLower) * SPB * 2))) { // 4/1 beat ticks
        timeInterval.minor = 60 * 2 / (BPM * (timeSigLower / 8));
        timeInterval.major = (60 * timeSigUpper) / (BPM * (timeSigLower / 16));
        timeInterval.minorMinor = 60 / (BPM * (timeSigLower / 8));
        timeInterval.digits = -1; // do not draw labels for minor ticks

        return timeInterval;
    }

    int MAX_BEATS_ZOOM_OUT = (4 / timeSigLower) * SPB * 8 * 32769;
    int factor = 2;
    while (static_cast<double>(60) * factor / BPM <= MAX_BEATS_ZOOM_OUT)
    {
        if (zoom > (12 / ((4 / timeSigLower) * ((static_cast<double>(60) * factor / BPM) / 2)))) {
            timeInterval.minor = std::numeric_limits<int>::max(); // do not draw minor ticks
            timeInterval.major = (60 * timeSigUpper) / (BPM * (timeSigLower / (4 * factor)));
            timeInterval.minorMinor = 60 / (BPM * (timeSigLower / (2 * factor)));

            return timeInterval;
        }
        factor *= 2;
    }

    timeInterval.minor = std::numeric_limits<int>::max(); // do not draw minor ticks
    timeInterval.major = (60 * timeSigUpper) / (BPM * (timeSigLower / (4 * factor)));
    timeInterval.minorMinor = 60 / (BPM * (timeSigLower / (2 * factor)));

    return timeInterval;
}

QString BeatsMeasuresFormat::label(double d, const IntervalInfo& interval, TickType tickType, TimelineContext* context)
{
    if (tickType == TickType::MINORMINOR) {
        return QString();
    }

    if (d < 0.0 && (d + interval.minor > 0.0)) {
        d = 0.0;
    }

    int BPM = context->BPM();
    // Seconds Per Beat
    double SPB = static_cast<double>(60) / BPM;
    int timeSigUpper = context->timeSigUpper();
    int timeSigLower = context->timeSigLower();

    const auto lower = static_cast<double>(timeSigLower);
    double val = (BPM * (lower / 4) * d) / (60 * timeSigUpper);
    double beatApprox = (val - floor(val)) * timeSigUpper + 1;
    int beat = round(beatApprox);

    if (interval.minorMinor >= SPB * (4 / timeSigLower)) {
        if (interval.digits == -1 && tickType == TickType::MINOR) {
            return QString();
        } else if (tickType == TickType::MINOR && beat != 1 && abs(beat - beatApprox) <= 1.0e-5f) {
            return QString("%1.%2")
                   .arg(QString::number(static_cast<int>(floor(val + 1))))
                   .arg(QString::number(static_cast<int>(beat)));
        } else if (abs(beat - beatApprox) > 1.0e-5f) {
            return QString();
        }
        return QString::number(static_cast<int>(round(val + 1)));
    } else if (interval.minorMinor >= (SPB / 2) * (4 / timeSigLower)) {
        if (tickType == TickType::MAJOR) {
            return QString::number(static_cast<int>(round(val + 1)));
        }
        return QString("%1.%2")
               .arg(QString::number(static_cast<int>(floor(val + 1))))
               .arg(QString::number(static_cast<int>(beat)));
    } else {
        if (tickType == TickType::MINOR) {
            if (abs(beat - beatApprox) > 1.0e-5f) {
                return QString();
            }
        }
        if (tickType == TickType::MAJOR) {
            return QString::number(static_cast<int>(round(val + 1)));
        }
        return QString("%1.%2")
               .arg(QString::number(static_cast<int>(floor(val + 1))))
               .arg(QString::number(static_cast<int>(beat)));
    }

    return QString();
}
