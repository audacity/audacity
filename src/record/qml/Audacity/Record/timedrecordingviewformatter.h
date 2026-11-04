/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <chrono>
#include <optional>

#include <QDateTime>
#include <QString>

namespace au::record {
class TimedRecordingViewFormatter
{
public:
    //! E.g. "1h 30m", "2d", "17h 36m 30s"
    static QString duration(qint64 seconds);

    //! E.g. "05:00", "59:55", "01:00:00"
    static QString clock(qint64 seconds);

    //! E.g. "Sat 24 Oct 2026 at 12:23:30"
    static QString dateTime(const QDateTime& dateTime);

    //! The line summing up when the recording starts and ends or how long it lasts.
    //! `start` is empty when the recording starts as soon as the user confirms.
    static QString summary(const QDateTime& now, const std::optional<QDateTime>& start, qint64 durationSeconds, bool endSetByDuration);

    static QString startTimePassedError(const QDateTime& now);
    static QString endTimePassedError(const QDateTime& now);
    static QString endBeforeStartError();

    static QDateTime toDateTime(std::chrono::system_clock::time_point timePoint);
    static std::chrono::system_clock::time_point toTimePoint(const QDateTime& dateTime);

private:
    static QString startPhrase(const QDateTime& now, const QDateTime& start);
    static QString dayAndTime(const QDateTime& now, const QDateTime& dateTime);
    static QString time(const QDateTime& dateTime);
};
}
