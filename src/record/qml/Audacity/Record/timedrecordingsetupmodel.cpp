/*
* Audacity: A Digital Audio Editor
*/
#include "timedrecordingsetupmodel.h"

#include <QLocale>
#include <QRegularExpression>

#include "uicomponents/types/numerictypes.h"

#include "timedrecordingviewformatter.h"

using namespace au::record;

namespace {
constexpr int CLOCK_INTERVAL_MS = 1000;
constexpr qint64 DEFAULT_DURATION_SECONDS = 60 * 60;

const QString TIME_FORMAT("HH:mm:ss");
const QStringList ALTERNATIVE_TIME_FORMATS { "H:mm:ss", "H:mm" };

QString numericDateFormat()
{
    QString format = QLocale().dateFormat(QLocale::ShortFormat);
    format.replace(QRegularExpression("y+"), "yyyy");
    format.replace(QRegularExpression("M+"), "MM");
    format.replace(QRegularExpression("d+"), "dd");
    return format;
}

QDateTime currentDateTime()
{
    QDateTime now = QDateTime::currentDateTime();
    now.setTime(QTime(now.time().hour(), now.time().minute(), now.time().second()));
    return now;
}
}

TimedRecordingSetupModel::TimedRecordingSetupModel(QObject* parent)
    : QObject(parent), m_dateFormat(numericDateFormat())
{
    m_clock.setInterval(CLOCK_INTERVAL_MS);
    connect(&m_clock, &QTimer::timeout, this, &TimedRecordingSetupModel::update);
    connect(this, &TimedRecordingSetupModel::scheduleChanged, this, &TimedRecordingSetupModel::update);
}

void TimedRecordingSetupModel::init()
{
    const QDateTime now = currentDateTime();
    const QDateTime end = now.addSecs(DEFAULT_DURATION_SECONDS);

    m_startDate = now.date().toString(m_dateFormat);
    m_startTime = now.time().toString(TIME_FORMAT);
    m_durationSeconds = DEFAULT_DURATION_SECONDS;
    m_endDate = end.date().toString(m_dateFormat);
    m_endTime = end.time().toString(TIME_FORMAT);

    emit scheduleChanged();
    m_clock.start();
}

QVariantMap TimedRecordingSetupModel::scheduleData() const
{
    const std::optional<TimedRecordingSchedule> s = schedule();
    if (!s) {
        return {};
    }

    const auto msSinceEpoch = [](TimedRecordingSchedule::TimePoint timePoint) {
        return static_cast<qint64>(std::chrono::duration_cast<std::chrono::milliseconds>(timePoint.time_since_epoch()).count());
    };

    QVariantMap data;
    if (s->start) {
        data["startMs"] = msSinceEpoch(*s->start);
    }
    if (const TimedRecordingSchedule::TimePoint* end = std::get_if<TimedRecordingSchedule::TimePoint>(&s->end)) {
        data["endMs"] = msSinceEpoch(*end);
    }
    data["durationSeconds"] = static_cast<qint64>(m_durationSeconds);
    return data;
}

bool TimedRecordingSetupModel::startNow() const
{
    return m_startNow;
}

void TimedRecordingSetupModel::setStartNow(bool startNow)
{
    set(m_startNow, startNow);
}

QString TimedRecordingSetupModel::startDate() const
{
    return m_startDate;
}

void TimedRecordingSetupModel::setStartDate(const QString& date)
{
    set(m_startDate, date);
}

QString TimedRecordingSetupModel::startTime() const
{
    return m_startTime;
}

void TimedRecordingSetupModel::setStartTime(const QString& time)
{
    set(m_startTime, time);
}

bool TimedRecordingSetupModel::endAfterDuration() const
{
    return m_endAfterDuration;
}

void TimedRecordingSetupModel::setEndAfterDuration(bool endAfterDuration)
{
    set(m_endAfterDuration, endAfterDuration);
}

double TimedRecordingSetupModel::durationSeconds() const
{
    return m_durationSeconds;
}

void TimedRecordingSetupModel::setDurationSeconds(double seconds)
{
    set(m_durationSeconds, seconds);
}

QString TimedRecordingSetupModel::endDate() const
{
    return m_endDate;
}

void TimedRecordingSetupModel::setEndDate(const QString& date)
{
    set(m_endDate, date);
}

QString TimedRecordingSetupModel::endTime() const
{
    return m_endTime;
}

void TimedRecordingSetupModel::setEndTime(const QString& time)
{
    set(m_endTime, time);
}

QString TimedRecordingSetupModel::dateFormat() const
{
    return m_dateFormat;
}

int TimedRecordingSetupModel::durationFormat() const
{
    return static_cast<int>(uicomponents::TimecodeFormatType::DDHHMMSS);
}

QString TimedRecordingSetupModel::summary() const
{
    return m_summary;
}

bool TimedRecordingSetupModel::canStart() const
{
    return m_canStart;
}

template<typename T>
void TimedRecordingSetupModel::set(T& member, const T& value)
{
    if (member == value) {
        return;
    }
    member = value;
    emit scheduleChanged();
}

std::optional<TimedRecordingSchedule> TimedRecordingSetupModel::schedule() const
{
    TimedRecordingSchedule s;
    s.end = TimedRecordingSchedule::Duration(static_cast<int64_t>(m_durationSeconds));

    if (!m_startNow) {
        const std::optional<QDateTime> start = parseDateTime(m_startDate, m_startTime);
        if (!start) {
            return std::nullopt;
        }
        s.start = TimedRecordingViewFormatter::toTimePoint(*start);
    }

    if (!m_endAfterDuration) {
        const std::optional<QDateTime> end = parseDateTime(m_endDate, m_endTime);
        if (!end) {
            return std::nullopt;
        }
        s.end = TimedRecordingViewFormatter::toTimePoint(*end);
    }

    return s;
}

std::optional<QDateTime> TimedRecordingSetupModel::parseDateTime(const QString& date, const QString& time) const
{
    const QDate d = QDate::fromString(date.trimmed(), m_dateFormat);
    if (!d.isValid()) {
        return std::nullopt;
    }

    QTime t = QTime::fromString(time.trimmed(), TIME_FORMAT);
    for (const QString& format : ALTERNATIVE_TIME_FORMATS) {
        if (t.isValid()) {
            break;
        }
        t = QTime::fromString(time.trimmed(), format);
    }
    if (!t.isValid()) {
        return std::nullopt;
    }

    return QDateTime(d, t);
}

void TimedRecordingSetupModel::update()
{
    const QDateTime now = currentDateTime();
    const std::optional<TimedRecordingSchedule> s = schedule();
    const TimedRecordingSchedule::TimePoint nowTimePoint = TimedRecordingViewFormatter::toTimePoint(now);

    const bool canStart = s && s->isValid(nowTimePoint);
    QString summary;
    if (canStart) {
        const std::optional<QDateTime> start = s->start ? std::optional(TimedRecordingViewFormatter::toDateTime(*s->start)) : std::nullopt;
        summary = TimedRecordingViewFormatter::summary(now, start, s->recordingDuration(nowTimePoint).count(), m_endAfterDuration);
    }

    if (canStart == m_canStart && summary == m_summary) {
        return;
    }

    m_canStart = canStart;
    m_summary = summary;
    emit summaryChanged();
}
