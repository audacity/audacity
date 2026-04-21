/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QtQml/qqmlregistration.h>

#include "modularity/ioc.h"

#include "async/asyncable.h"

#include "record/irecordconfiguration.h"

namespace au::appshell {
class RecordingPreferencesModel : public QObject, public muse::async::Asyncable
{
    Q_OBJECT
    QML_ELEMENT

    Q_PROPERTY(bool isInputMonitoringOn READ isInputMonitoringOn WRITE setIsInputMonitoringOn NOTIFY isInputMonitoringOnChanged)
    Q_PROPERTY(bool isMicMeteringOn READ isMicMeteringOn WRITE setIsMicMeteringOn NOTIFY isMicMeteringOnChanged)
    Q_PROPERTY(double leadInTimeDuration READ leadInTimeDuration NOTIFY leadInTimeDurationChanged)
    Q_PROPERTY(double crossfadeDuration READ crossfadeDuration NOTIFY crossfadeDurationChanged)

    muse::GlobalInject<record::IRecordConfiguration> recordConfiguration;

public:
    explicit RecordingPreferencesModel(QObject* parent = nullptr);

    bool isInputMonitoringOn() const;
    void setIsInputMonitoringOn(bool enabled);

    bool isMicMeteringOn() const;
    void setIsMicMeteringOn(bool enabled);

    double leadInTimeDuration() const;
    Q_INVOKABLE void setLeadInTimeDuration(double seconds);

    double crossfadeDuration() const;
    Q_INVOKABLE void setCrossfadeDuration(double milliseconds);

signals:
    void isInputMonitoringOnChanged();
    void isMicMeteringOnChanged();
    void leadInTimeDurationChanged();
    void crossfadeDurationChanged();
};
}
