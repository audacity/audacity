/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>
#include <QtQml/qqmlregistration.h>

#include "framework/global/modularity/ioc.h"
#include "au3cloud/iusageinfo.h"

namespace au::appshell {
class AppUpdateUsageInfoPageModel : public QObject, public muse::Contextable
{
    Q_OBJECT
    QML_ELEMENT

    muse::ContextInject<au::au3cloud::IUsageInfo> usageInfo { this };

public:
    explicit AppUpdateUsageInfoPageModel(QObject* parent = nullptr);

    Q_INVOKABLE void setSendAnonymousUsageInfo(bool allow);
};
}
