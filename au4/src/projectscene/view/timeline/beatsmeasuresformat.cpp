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

    //! TODO AU4: may need update to depend on upper&lower measure
    if (zoom > (28 / (SPB / 32))) { // 1/128 ticks
        timeInterval.minor = SPB / 8;
        timeInterval.major = SPB * 4;
        timeInterval.minorMinor = (SPB / 32);
        return timeInterval;
    }
    if (zoom > (32 / (SPB / 16))) { // 1/64 ticks
        timeInterval.minor = SPB / 4;
        timeInterval.major = SPB * 4;
        timeInterval.minorMinor = (SPB / 16);
        return timeInterval;
    }
    if (zoom > (32 / (SPB / 8))) { // 1/32 beat ticks
        timeInterval.minor = SPB / 2;
        timeInterval.major = SPB * 4;
        timeInterval.minorMinor = SPB / 8;
        return timeInterval;
    }
    if (zoom > (14 / (SPB / 4))) { // 1/16 beat ticks
        timeInterval.minor = SPB;
        timeInterval.major = SPB * 4;
        timeInterval.minorMinor = SPB / 4;
        return timeInterval;
    }
    if (zoom > (14 / (SPB / 2))) {  // 1/8 beat ticks
        timeInterval.minor = SPB;
        timeInterval.major = SPB * 4;
        timeInterval.minorMinor = SPB / 2;
        return timeInterval;
    }

    if (zoom > (6 / (SPB))) { // 1/4 beat ticks
        timeInterval.minor = std::numeric_limits<int>::max(); // do not draw minor ticks
        timeInterval.major = SPB * 4;
        timeInterval.minorMinor = SPB;
        return timeInterval;
    }
    if (zoom > (6 / (SPB * 2))) { // 4/1 beat ticks
        timeInterval.minor = SPB * 4;
        timeInterval.major = SPB * 16;
        timeInterval.minorMinor = SPB * 2;
        timeInterval.digits = -1; // do not draw labels for minor ticks
        return timeInterval;
    }

    int MAX_BEATS_ZOOM_OUT = SPB * 8 * 32769;
    SPB *= 2;
    while (SPB < MAX_BEATS_ZOOM_OUT)
    {
        if (zoom > (6 / (SPB / 2))) {
            timeInterval.minor = std::numeric_limits<int>::max(); // do not draw minor ticks
            timeInterval.major = SPB * 4;
            timeInterval.minorMinor = SPB / 2;
            return timeInterval;
        }
        SPB *= 2;
    }

    timeInterval.minor = std::numeric_limits<int>::max(); // do not draw minor ticks
    timeInterval.major = SPB * 4;
    timeInterval.minorMinor = SPB;
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

    if (interval.minorMinor >= SPB) {
        if (interval.digits == -1 && tickType == TickType::MINOR) {
            return QString();
        }
        return QString::number(static_cast<int>(round(val + 1)));
    } else if (interval.minorMinor >= (SPB / 2)) {
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
