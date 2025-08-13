/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "modularity/ioc.h"

#include "async/asyncable.h"
#include "audio/iaudioconfiguration.h"

#include "record/irecord.h"

namespace au::appshell {
class RecordingPreferencesModel : public QObject, public muse::async::Asyncable
{
    Q_OBJECT

    Q_PROPERTY(bool audibleInputMonitoring READ audibleInputMonitoring WRITE setAudibleInputMonitoring NOTIFY audibleInputMonitoringChanged)

    muse::Inject<record::IRecord> record;

public:
    explicit RecordingPreferencesModel(QObject* parent = nullptr);

    bool audibleInputMonitoring() const;
    void setAudibleInputMonitoring(bool enabled);

signals:
    void audibleInputMonitoringChanged();
};
}
