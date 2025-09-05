/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "modularity/ioc.h"

#include "async/asyncable.h"
#include "audio/main/iaudioconfiguration.h"

#include "record/irecord.h"
#include "record/irecordconfiguration.h"

namespace au::appshell {
class RecordingPreferencesModel : public QObject, public muse::async::Asyncable
{
    Q_OBJECT

    Q_PROPERTY(bool audibleInputMonitoring READ audibleInputMonitoring WRITE setAudibleInputMonitoring NOTIFY audibleInputMonitoringChanged)
    Q_PROPERTY(bool isMicMeteringOn READ isMicMeteringOn WRITE setIsMicMeteringOn NOTIFY isMicMeteringOnChanged)

    muse::Inject<record::IRecord> record;
    muse::Inject<record::IRecordConfiguration> recordConfiguration;

public:
    explicit RecordingPreferencesModel(QObject* parent = nullptr);

    bool audibleInputMonitoring() const;
    void setAudibleInputMonitoring(bool enabled);

    bool isMicMeteringOn() const;
    void setIsMicMeteringOn(bool enabled);

signals:
    void audibleInputMonitoringChanged();
    void isMicMeteringOnChanged();
};
}
