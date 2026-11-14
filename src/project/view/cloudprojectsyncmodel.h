/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>

#include "global/async/asyncable.h"
#include "modularity/ioc.h"
#include "au3cloud/iau3audiocomservice.h"

namespace au::project {
class CloudProjectSyncModel : public QObject, public muse::async::Asyncable, public muse::Contextable
{
    Q_OBJECT

    muse::ContextInject<au3cloud::IAu3AudioComService> audioComService{ this };

    Q_PROPERTY(bool syncing READ syncing NOTIFY syncingChanged FINAL)

public:
    explicit CloudProjectSyncModel(QObject* parent = nullptr);

    Q_INVOKABLE void load();

    bool syncing() const;

signals:
    void syncingChanged();
};
}
