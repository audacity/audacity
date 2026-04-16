/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QObject>
#include <QtQml/qqmlregistration.h>

#include "modularity/ioc.h"
#include "au3cloud/iusageinfo.h"

namespace au::appshell {
class UsageInfoPreferencesModel : public QObject
{
    Q_OBJECT
    QML_ELEMENT

    muse::GlobalInject<au::au3cloud::IUsageInfo> usageInfo;

    Q_PROPERTY(bool sendAnonymousUsageInfo READ sendAnonymousUsageInfo WRITE setSendAnonymousUsageInfo NOTIFY sendAnonymousUsageInfoChanged)

public:
    explicit UsageInfoPreferencesModel(QObject* parent = nullptr);

    bool sendAnonymousUsageInfo() const;

public slots:
    void setSendAnonymousUsageInfo(bool allow);

signals:
    void sendAnonymousUsageInfoChanged(bool allow);
};
}
