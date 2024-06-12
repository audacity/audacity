/**********************************************************************

  Audacity: A Digital Audio Editor

**********************************************************************/

#include "timeformat.h"

using namespace au::projectscene;

IntervalInfo TimeFormat::intervalInfo(TimelineContext* context)
{
    IntervalInfo timeInterval;
    double zoom = context->zoom();
    // NOTE: 24 is minimal px size between minorMinor ticks
    // 0.000005 is distance [s] between minorMinor ticks
    // same goes for other time intervals
    if (zoom > (24 / 0.000005)) { // 0.000005s ticks
        timeInterval.minor = 0.00002;
        timeInterval.major = 0.0001;
        timeInterval.minorMinor = 0.000005;
        timeInterval.digits = 6;
        return timeInterval;
    }
    if (zoom > (28 / 0.00001)) { // 0.00001s ticks
        timeInterval.minor = 0.000005;
        timeInterval.major = 0.0005;
        timeInterval.minorMinor = 0.00001;
        timeInterval.digits = 6;
        return timeInterval;
    }
    if (zoom > (24 / 0.00005)) { // 0.00005s ticks
        timeInterval.minor = 0.0002;
        timeInterval.major = 0.001;
        timeInterval.minorMinor = 0.00005;
        timeInterval.digits = 5;
        return timeInterval;
    }
    if (zoom > (24 / 0.0001)) { // 0.0001s ticks
        timeInterval.minor = 0.0005;
        timeInterval.major = 0.005;
        timeInterval.minorMinor = 0.0001;
        timeInterval.digits = 4;
        return timeInterval;
    }
    if (zoom > (32 / 0.0005)) { // 0.0005s ticks
        timeInterval.minor = 0.001;
        timeInterval.major = 0.005;
        timeInterval.minorMinor = 0.0005;
        timeInterval.digits = 4;
        return timeInterval;
    }
    if (zoom > (24 / 0.001)) { // 0.001s ticks
        timeInterval.minor = 0.005;
        timeInterval.major = 0.05;
        timeInterval.minorMinor = 0.001;
        timeInterval.digits = 3;
        return timeInterval;
    }
    if (zoom > (24 / 0.005)) { // 0.005s ticks
        timeInterval.minor = 0.01;
        timeInterval.major = 0.05;
        timeInterval.minorMinor = 0.005;
        timeInterval.digits = 3;
        return timeInterval;
    }
    if (zoom > (24 / 0.01)) { // 0.01s ticks
        timeInterval.minor = 0.05;
        timeInterval.major = 0.1;
        timeInterval.minorMinor = 0.01;
        timeInterval.digits = 2;
        return timeInterval;
    }
    if (zoom > (24 / 0.05)) { // 0.05s ticks
        timeInterval.minor = 0.1;
        timeInterval.major = 0.5;
        timeInterval.minorMinor = 0.05;
        timeInterval.digits = 2;
        return timeInterval;
    }
    if (zoom > (24 / 0.1)) { // 0.1s ticks
        timeInterval.minor = 0.5;
        timeInterval.major = 1.0;
        timeInterval.minorMinor = 0.1;
        timeInterval.digits = 1;
        return timeInterval;
    }
    if (zoom > (28 / 0.5)) { // 0.5s ticks
        timeInterval.minor = 1.0;
        timeInterval.major = 5.0;
        timeInterval.minorMinor = 0.5;
        return timeInterval;
    }
    if (zoom > (24 / 1.0)) { // 1s ticks
        timeInterval.minor = 5.0;
        timeInterval.major = 30.0;
        timeInterval.minorMinor = 1.0;
        return timeInterval;
    }
    if (zoom > (24 / 5.0)) { // 5s ticks
        timeInterval.minor = 15;
        timeInterval.major = 60;
        timeInterval.minorMinor = 5.0;
        return timeInterval;
    }
    if (zoom > (24 / 10.0)) { // 10s ticks
        timeInterval.minor = 30;
        timeInterval.major = 60;
        timeInterval.minorMinor = 10.0;
        return timeInterval;
    }
    if (zoom > (24 / 15.0)) { // 15s ticks
        timeInterval.minor = 60;
        timeInterval.major = 300;
        timeInterval.minorMinor = 15.0;
        return timeInterval;
    }
    if (zoom > (28 / 30.0)) { // 30s ticks
        timeInterval.minor = 60.0;
        timeInterval.major = 300;
        timeInterval.minorMinor = 30.0;
        return timeInterval;
    }
    if (zoom > (22 / 60.0)) { // 1 min ticks
        timeInterval.minor = 300;
        timeInterval.major = 900;
        timeInterval.minorMinor = 60.0;
        return timeInterval;
    }
    if (zoom > (24 / 300.0)) { // 5 min ticks
        timeInterval.minor = 900;
        timeInterval.major = 3600;
        timeInterval.minorMinor = 5 * 60.0;
        return timeInterval;
    }
    if (zoom > (24 / 600.0)) { // 10 min ticks
        timeInterval.minor = 1800;
        timeInterval.major = 3600;
        timeInterval.minorMinor = 10 * 60.0;
        return timeInterval;
    }
    if (zoom > (24 / 900.0)) { // 15 min ticks
        timeInterval.minor = 3600.0;
        timeInterval.major = 5 * 3600.0;
        timeInterval.minorMinor = 15 * 60.0;
        return timeInterval;
    }
    if (zoom > (24 / 1800.0)) { // 30 min ticks
        timeInterval.minor = 3600.0;
        timeInterval.major = 5 * 3600.0;
        timeInterval.minorMinor = 30 * 60.0;
        return timeInterval;
    }
    if (zoom > (24 / 3600.0)) { // 1 h ticks
        timeInterval.minor = 6 * 3600.0;
        timeInterval.major = 24 * 3600.0;
        timeInterval.minorMinor = 3600.0;
        return timeInterval;
    }
    if (zoom > (24 / (6 * 3600.0))) { // 6 h ticks
        timeInterval.minor = 24 * 3600.0;
        timeInterval.major = 168 * 3600.0;
        timeInterval.minorMinor = 6 * 3600.0;
        return timeInterval;
    }

    timeInterval.minor = 24 * 3600 + 1; // HACK to not to draw minor ticks labels
    timeInterval.major = 7 * 24 * 3600.0;
    timeInterval.minorMinor = 24 * 3600.0;
    return timeInterval;
}

QString TimeFormat::label(double d, const IntervalInfo& intervalInfo, TickType tickType)
{
    // Replace -0 with 0
    if (d < 0.0 && (d + intervalInfo.minor > 0.0)) {
        d = 0.0;
    }

    if (tickType == TickType::MAJOR || tickType == TickType::MINOR) {
        if (d < 0) {
            return QString();
        }

        if (intervalInfo.minor >= 3600.0) {
            int hrs = (int)(d / 3600.0 + 0.5);
            return QString("%1:00:00").arg(hrs);
        } else if (intervalInfo.minor >= 60.0) {
            int minutes = (int)(d / 60.0 + 0.5);
            if (minutes >= 60) {
                return QTime(minutes / 60, minutes % 60).toString("HH:mm:ss");
            } else {
                return QTime(0, minutes).toString("mm:ss");
            }
        } else if (intervalInfo.minor > 0.5) {
            int secs = (int)(d + 0.5);
            if (secs >= 3600) {
                return QTime(secs / 3600, (secs / 60) % 60, secs % 60).toString("HH:mm:ss");
            } else if (secs >= 60) {
                return QTime(0, secs / 60, secs % 60).toString("mm:ss");
            } else {
                return QTime(0, 0, secs).toString("m:ss");
            }
        } else {
            if (intervalInfo.minor >= 1.0) {
                return QString("%1").arg(static_cast<int>(floor(d + 0.5)));
            } else {
                return QString::number(d, 'f', intervalInfo.digits);
            }
        }
    }

    return QString();
}
