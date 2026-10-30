/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <optional>

#include <QDateTime>
#include <QObject>
#include <QTimer>
#include <QVariantMap>
#include <QtQml/qqmlregistration.h>

#include "record/timedrecordingtypes.h"

namespace au::record {
class TimedRecordingSetupModel : public QObject
{
    Q_OBJECT
    QML_ELEMENT;

    Q_PROPERTY(bool startNow READ startNow WRITE setStartNow NOTIFY scheduleChanged FINAL)
    Q_PROPERTY(QString startDate READ startDate WRITE setStartDate NOTIFY scheduleChanged FINAL)
    Q_PROPERTY(QString startTime READ startTime WRITE setStartTime NOTIFY scheduleChanged FINAL)

    Q_PROPERTY(bool endAfterDuration READ endAfterDuration WRITE setEndAfterDuration NOTIFY scheduleChanged FINAL)
    Q_PROPERTY(double durationSeconds READ durationSeconds WRITE setDurationSeconds NOTIFY scheduleChanged FINAL)
    Q_PROPERTY(QString endDate READ endDate WRITE setEndDate NOTIFY scheduleChanged FINAL)
    Q_PROPERTY(QString endTime READ endTime WRITE setEndTime NOTIFY scheduleChanged FINAL)

    Q_PROPERTY(QString dateFormat READ dateFormat CONSTANT FINAL)
    Q_PROPERTY(int durationFormat READ durationFormat CONSTANT FINAL)

    Q_PROPERTY(QString summary READ summary NOTIFY summaryChanged FINAL)
    Q_PROPERTY(bool canStart READ canStart NOTIFY summaryChanged FINAL)

public:
    explicit TimedRecordingSetupModel(QObject* parent = nullptr);

    Q_INVOKABLE void init();
    Q_INVOKABLE QVariantMap scheduleData() const;

    bool startNow() const;
    void setStartNow(bool startNow);
    QString startDate() const;
    void setStartDate(const QString& date);
    QString startTime() const;
    void setStartTime(const QString& time);

    bool endAfterDuration() const;
    void setEndAfterDuration(bool endAfterDuration);
    double durationSeconds() const;
    void setDurationSeconds(double seconds);
    QString endDate() const;
    void setEndDate(const QString& date);
    QString endTime() const;
    void setEndTime(const QString& time);

    QString dateFormat() const;
    int durationFormat() const;

    QString summary() const;
    bool canStart() const;

signals:
    void scheduleChanged();
    void summaryChanged();

private:
    template<typename T>
    void set(T& member, const T& value);

    std::optional<TimedRecordingSchedule> schedule() const;
    std::optional<QDateTime> parseDateTime(const QString& date, const QString& time) const;
    void update();

    bool m_startNow = true;
    QString m_startDate;
    QString m_startTime;

    bool m_endAfterDuration = true;
    double m_durationSeconds = 0;
    QString m_endDate;
    QString m_endTime;

    QString m_dateFormat;
    QString m_summary;
    bool m_canStart = false;

    QTimer m_clock;
};
}
