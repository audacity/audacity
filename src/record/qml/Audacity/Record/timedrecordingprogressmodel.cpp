/*
* Audacity: A Digital Audio Editor
*/
#include "timedrecordingprogressmodel.h"

#include <algorithm>
#include <chrono>

#include <QDateTime>

#include "framework/global/translation.h"

#include "timedrecordingviewformatter.h"

using namespace au::record;

namespace {
constexpr int REFRESH_INTERVAL_MS = 200;
constexpr qint64 MS_PER_SECOND = 1000;

qint64 secondsUntil(const QDateTime& from, const QDateTime& to)
{
    const qint64 ms = from.msecsTo(to);
    return ms <= 0 ? 0 : (ms + MS_PER_SECOND - 1) / MS_PER_SECOND;
}
}

TimedRecordingProgressModel::TimedRecordingProgressModel(QObject* parent)
    : QObject(parent), muse::Contextable(muse::iocCtxForQmlObject(this))
{
    m_refresher.setInterval(REFRESH_INTERVAL_MS);
    connect(&m_refresher, &QTimer::timeout, this, &TimedRecordingProgressModel::update);
}

void TimedRecordingProgressModel::init()
{
    scenario()->stateChanged().onNotify(this, [this]() {
        update();
    });

    update();
    m_refresher.start();
}

void TimedRecordingProgressModel::stop()
{
    scenario()->requestStop();
}

bool TimedRecordingProgressModel::isRecording() const
{
    return m_isRecording;
}

bool TimedRecordingProgressModel::isQuitting() const
{
    return m_isQuitting;
}

QString TimedRecordingProgressModel::statusText() const
{
    return m_statusText;
}

QString TimedRecordingProgressModel::clockText() const
{
    return m_clockText;
}

QString TimedRecordingProgressModel::startText() const
{
    return m_startText;
}

QString TimedRecordingProgressModel::endText() const
{
    return m_endText;
}

void TimedRecordingProgressModel::update()
{
    const QDateTime now = QDateTime::currentDateTime();
    const QDateTime start = TimedRecordingViewFormatter::toDateTime(scenario()->startTime());
    const QDateTime end = TimedRecordingViewFormatter::toDateTime(scenario()->endTime());
    const ITimedRecordingScenario::State state = scenario()->state();
    const bool isRecording = state == ITimedRecordingScenario::State::Recording;
    const bool isQuitting = state == ITimedRecordingScenario::State::Quitting;

    QString statusText;
    QString clockText;
    QString startText;
    if (isQuitting) {
        const qint64 secondsToQuit = secondsUntil(now, TimedRecordingViewFormatter::toDateTime(scenario()->quitTime()));
        //: %1 is the time left until Audacity quits, e.g. "59s"
        statusText = muse::qtrc("record", "Audacity will quit in %1").arg(TimedRecordingViewFormatter::duration(secondsToQuit));
        clockText = TimedRecordingViewFormatter::clock(secondsToQuit);
    } else if (isRecording) {
        //: %1 is the time left until the timed recording ends, e.g. "59m 50s"
        statusText = muse::qtrc("record", "Recording ends in %1").arg(TimedRecordingViewFormatter::duration(secondsUntil(now, end)));
        clockText = TimedRecordingViewFormatter::clock(start.secsTo(now));
        //: %1 is a date and a time, e.g. "Sat 24 Oct 2026 at 12:23:30"
        startText = muse::qtrc("record", "Started: %1").arg(TimedRecordingViewFormatter::dateTime(start));
    } else {
        const qint64 secondsToStart = std::max<qint64>(secondsUntil(now, start), 1);
        //: %1 is the time left until the timed recording starts, e.g. "5m"
        statusText = muse::qtrc("record", "Recording starts in %1").arg(TimedRecordingViewFormatter::duration(secondsToStart));
        clockText = TimedRecordingViewFormatter::clock(secondsToStart);
        //: %1 is a date and a time, e.g. "Sat 24 Oct 2026 at 12:23:30"
        startText = muse::qtrc("record", "Starts: %1").arg(TimedRecordingViewFormatter::dateTime(start));
    }
    //: %1 is a date and a time, e.g. "Sat 24 Oct 2026 at 13:23:30"
    const QString endText = muse::qtrc("record", "Ends: %1").arg(TimedRecordingViewFormatter::dateTime(end));

    if (isRecording == m_isRecording && isQuitting == m_isQuitting && statusText == m_statusText && clockText == m_clockText
        && startText == m_startText && endText == m_endText) {
        return;
    }

    m_isRecording = isRecording;
    m_isQuitting = isQuitting;
    m_statusText = statusText;
    m_clockText = clockText;
    m_startText = startText;
    m_endText = endText;
    emit changed();
}
