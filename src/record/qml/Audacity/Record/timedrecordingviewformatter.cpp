/*
* Audacity: A Digital Audio Editor
*/
#include "timedrecordingviewformatter.h"

#include <algorithm>

#include <QLocale>

#include "framework/global/translation.h"

#include "record/internal/timedrecordingformatter.h"

using namespace au::record;

namespace {
constexpr qint64 SECONDS_PER_MINUTE = 60;
constexpr qint64 SECONDS_PER_HOUR = 60 * SECONDS_PER_MINUTE;
}

QString TimedRecordingViewFormatter::duration(qint64 seconds)
{
    return TimedRecordingFormatter::duration(std::chrono::seconds(seconds)).toQString();
}

QString TimedRecordingViewFormatter::clock(qint64 seconds)
{
    seconds = std::max<qint64>(seconds, 0);

    const qint64 hours = seconds / SECONDS_PER_HOUR;
    const qint64 minutes = seconds % SECONDS_PER_HOUR / SECONDS_PER_MINUTE;
    const qint64 secs = seconds % SECONDS_PER_MINUTE;

    const QString minutesAndSeconds = QString("%1:%2").arg(minutes, 2, 10, QChar('0')).arg(secs, 2, 10, QChar('0'));
    if (hours == 0) {
        return minutesAndSeconds;
    }

    return QString("%1:%2").arg(hours, 2, 10, QChar('0')).arg(minutesAndSeconds);
}

QString TimedRecordingViewFormatter::dateTime(const QDateTime& dateTime)
{
    const QString date = QLocale().toString(dateTime.date(), "ddd d MMM yyyy");
    //: A date and a time, e.g. "Sat 24 Oct 2026 at 12:23:30"
    return muse::qtrc("record", "%1 at %2").arg(date, time(dateTime));
}

QString TimedRecordingViewFormatter::summary(const QDateTime& now, const std::optional<QDateTime>& start, qint64 durationSeconds,
                                             bool endSetByDuration)
{
    const QString durationText = duration(durationSeconds);

    if (!start || *start <= now) {
        if (endSetByDuration) {
            const QDateTime end = now.addSecs(durationSeconds);
            //: %1 is when the recording ends, e.g. "today at 16:28:20", and %2 its duration, e.g. "1h"
            return muse::qtrc("record", "Starts when you press Start timed recording and ends %1 (%2).")
                   .arg(dayAndTime(now, end), durationText);
        }

        //: %1 is the duration of the recording, e.g. "1h 30m"
        return muse::qtrc("record", "Starts when you press Start timed recording and records for %1.").arg(durationText);
    }

    //: %1 is when the recording starts, e.g. "today at 12:23:30" or "in 43m 30s", and %2 its duration, e.g. "1h 30m"
    return muse::qtrc("record", "Starts %1 and records for %2.").arg(startPhrase(now, *start), durationText);
}

QString TimedRecordingViewFormatter::startTimePassedError(const QDateTime& now)
{
    //: %1 is the current time of day, e.g. "12:25:14"
    return muse::qtrc("record", "Start time has passed. Choose a time after %1.").arg(time(now));
}

QString TimedRecordingViewFormatter::endTimePassedError(const QDateTime& now)
{
    //: %1 is the current time of day, e.g. "12:25:14"
    return muse::qtrc("record", "End time has passed. Choose a time after %1.").arg(time(now));
}

QString TimedRecordingViewFormatter::endBeforeStartError()
{
    return muse::qtrc("record", "End time must be after the start time.");
}

QDateTime TimedRecordingViewFormatter::toDateTime(std::chrono::system_clock::time_point timePoint)
{
    return QDateTime::fromMSecsSinceEpoch(std::chrono::duration_cast<std::chrono::milliseconds>(timePoint.time_since_epoch()).count());
}

std::chrono::system_clock::time_point TimedRecordingViewFormatter::toTimePoint(const QDateTime& dateTime)
{
    return std::chrono::system_clock::time_point { std::chrono::milliseconds { dateTime.toMSecsSinceEpoch() } };
}

QString TimedRecordingViewFormatter::startPhrase(const QDateTime& now, const QDateTime& start)
{
    const qint64 secondsAway = now.secsTo(start);
    if (secondsAway < SECONDS_PER_HOUR) {
        //: %1 is how long until the recording starts, e.g. "43m 30s"
        return muse::qtrc("record", "in %1").arg(duration(secondsAway));
    }

    return dayAndTime(now, start);
}

QString TimedRecordingViewFormatter::dayAndTime(const QDateTime& now, const QDateTime& dateTime)
{
    const qint64 daysAway = now.date().daysTo(dateTime.date());
    if (daysAway == 0) {
        //: %1 is a time of day, e.g. "16:28:20"
        return muse::qtrc("record", "today at %1").arg(time(dateTime));
    }

    if (daysAway == 1) {
        //: %1 is a time of day, e.g. "16:28:20"
        return muse::qtrc("record", "tomorrow at %1").arg(time(dateTime));
    }

    //: %1 is a date and a time, e.g. "Sun 25 Oct 2026 at 16:28:20"
    return muse::qtrc("record", "on %1").arg(TimedRecordingViewFormatter::dateTime(dateTime));
}

QString TimedRecordingViewFormatter::time(const QDateTime& dateTime)
{
    return dateTime.time().toString("HH:mm:ss");
}
