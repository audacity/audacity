/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QObject>
#include <QtQml/qqmlregistration.h>

#include "modularity/ioc.h"
#include "diagnostics/idiagnosticsconfiguration.h"
#include "usageinfo/iusageinfo.h"

namespace au::appshell {
class UsageInfoPreferencesModel : public QObject
{
    Q_OBJECT
    QML_ELEMENT

    muse::GlobalInject<au::usageinfo::IUsageInfo> usageInfo;
    muse::GlobalInject<muse::diagnostics::IDiagnosticsConfiguration> diagnosticsConfiguration;

    Q_PROPERTY(bool sendAnonymousUsageInfo READ sendAnonymousUsageInfo WRITE setSendAnonymousUsageInfo NOTIFY sendAnonymousUsageInfoChanged)
    Q_PROPERTY(bool sendCrashReports READ sendCrashReports WRITE setSendCrashReports NOTIFY sendCrashReportsChanged)

public:
    explicit UsageInfoPreferencesModel(QObject* parent = nullptr);

    Q_INVOKABLE bool isCrashReportingAvailable() const;
    Q_INVOKABLE bool isUsageInfoAvailable() const;

    bool sendAnonymousUsageInfo() const;
    bool sendCrashReports() const;

public slots:
    void setSendAnonymousUsageInfo(bool allow);
    void setSendCrashReports(bool send);

signals:
    void sendAnonymousUsageInfoChanged(bool allow);
    void sendCrashReportsChanged(bool send);
};
}
