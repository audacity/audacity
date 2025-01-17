/**********************************************************************

  Audacity: A Digital Audio Editor

**********************************************************************/

#include "timeformat.h"

using namespace au::projectscene;

const std::vector<std::pair<double, IntervalInfo> > zoomToIntervalInfo = {
    // { min pixel width per minorMinor tick, {major, minor, minorMinor, digits} }
    { 32 / 0.000005, { 0.0001, 0.00002, 0.000005, 6 } },            // 0.000005s ticks
    { 28 / 0.00001, { 0.0005, 0.00005, 0.00001, 6 } },              // 0.00001s ticks
    { 32 / 0.00005, { 0.001, 0.0002, 0.00005, 5 } },                // 0.00005s ticks
    { 28 / 0.0001, { 0.005, 0.0005, 0.0001, 4 } },                  // 0.0001s ticks
    { 28 / 0.0005, { 0.005, 0.0025, 0.0005, 4 } },                  // 0.0005s ticks
    { 32 / 0.001, { 0.05, 0.005, 0.001, 3 } },                      // 0.001s ticks
    { 38 / 0.005, { 0.05, 0.01, 0.005, 3 } },                       // 0.005s ticks
    { 28 / 0.005, { 0.05, 0.025, 0.005, 3 } },                      // 0.005s ticks
    { 28 / 0.01, { 0.1, 0.05, 0.01, 2 } },                          // 0.01s ticks
    { 36 / 0.05, { 0.5, 0.1, 0.05, 2 } },                           // 0.05s ticks
    { 28 / 0.05, { 0.5, 0.25, 0.05, 2 } },                          // 0.05s ticks
    { 24 / 0.1, { 1.0, 0.5, 0.1, 1 } },                             // 0.1s ticks
    { 32 / 0.5, { 5.0, 1.0, 0.5, 0 } },                             // 0.5s ticks
    { 28 / 0.5, { 5.0, 2.5, 0.5, 0 } },                             // 0.5s ticks
    { 24 / 1.0, { 30.0, 5.0, 1.0, 0 } },                            // 1s ticks
    { 24 / 5.0, { 60.0, 15.0, 5.0, 0 } },                           // 5s ticks
    { 24 / 10.0, { 60.0, 30.0, 10.0, 0 } },                         // 10s ticks
    { 24 / 15.0, { 300.0, 60.0, 15.0, 0 } },                        // 15s ticks
    { 32 / 30.0, { 300.0, 60.0, 30.0, 0 } },                        // 30s ticks
    { 28 / 30.0, { 300.0, 150.0, 30.0, 0 } },                       // 30s ticks
    { 22 / 60.0, { 900.0, 300.0, 60.0, 0 } },                       // 1 min ticks
    { 24 / 300.0, { 3600.0, 900.0, 5 * 60.0, 0 } },                 // 5 min ticks
    { 24 / 600.0, { 3600.0, 1800.0, 10 * 60.0, 0 } },               // 10 min ticks
    { 24 / 900.0, { 5 * 3600.0, 3600.0, 15 * 60.0, 0 } },           // 15 min ticks
    { 32 / 1800.0, { 5 * 3600.0, 3600.0, 30 * 60.0, 0 } },          // 30 min ticks
    { 24 / 1800.0, { 5 * 3600.0, 2.5 * 3600.0, 30 * 60.0, 0 } },    // 30 min ticks
    { 24 / 3600.0, { 24 * 3600.0, 6 * 3600.0, 3600.0, 0 } },        // 1 h ticks
    { 24 / (6 * 3600.0), { 168 * 3600.0, 24 * 3600.0, 6 * 3600.0, 0 } }, // 6 h ticks
    { 0, { 24 * 3600 + 1, 7 * 24 * 3600.0, 24 * 3600.0, 0 } }       // fallback
};

IntervalInfo TimeFormat::intervalInfo(TimelineContext* context)
{
    double zoom = context->zoom();

    auto it = std::lower_bound(
        zoomToIntervalInfo.begin(),
        zoomToIntervalInfo.end(),
        zoom,
        [](const std::pair<double, IntervalInfo>& interval, double zoomValue) {
        return interval.first > zoomValue;
    }
        );

    if (it != zoomToIntervalInfo.end()) {
        return it->second;
    }

    return zoomToIntervalInfo.end()->second;
}

QString TimeFormat::label(double d, const IntervalInfo& intervalInfo, TickType tickType, TimelineContext* context)
{
    Q_UNUSED(context);
    // Replace -0 with 0
    if (d < 0.0 && (d + intervalInfo.minor > 0.0)) {
        d = 0.0;
    }

    if (tickType == TickType::MAJOR || tickType == TickType::MINOR) {
        if (d < 0) {
            return QString();
        }

        if (intervalInfo.minor >= 3600.0) {
            int secs = (int)(d + 0.5);
            return QString("%1:%2:00").arg(secs / 3600).arg((secs / 60) % 60, 2, 10, QChar('0'));
        } else if (intervalInfo.minor >= 60.0) {
            int minutes = (int)(d / 60.0 + 0.5);
            if (minutes >= 60) {
                return QString("%1:%2:00").arg(minutes / 60).arg(minutes % 60, 2, 10, QChar('0'));
            } else {
                return QString("%1:%2").arg(minutes)
                       .arg(0, 2, 10, QChar('0'));
            }
        } else if (intervalInfo.minor > 0.5) {
            int secs = (int)(d + 0.5);
            if (secs >= 3600) {
                return QString("%1:%2:%3")
                       .arg(secs / 3600)
                       .arg((secs / 60) % 60, 2, 10, QChar('0'))
                       .arg(secs % 60, 2, 10, QChar('0'));
            } else if (secs >= 60) {
                return QString("%1:%2")
                       .arg(secs / 60)
                       .arg(secs % 60, 2, 10, QChar('0'));
            } else {
                return QString("%1:%2")
                       .arg(secs / 60)
                       .arg(secs % 60, 2, 10, QChar('0'));
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
