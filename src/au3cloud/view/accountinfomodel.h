/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/global/async/asyncable.h"

#include "modularity/ioc.h"
#include "framework/interactive/iinteractive.h"
#include "au3cloud/iauthorization.h"

namespace au::au3cloud {
class AccountInfoModel : public QObject, public muse::async::Asyncable, public muse::Injectable
{
    Q_OBJECT

    muse::Inject<au::au3cloud::IAuthorization> authorization { this };
    muse::Inject<muse::IInteractive> interactive { this };

    Q_PROPERTY(bool isAuthorized READ isAuthorized NOTIFY isAuthorizedChanged)
    Q_PROPERTY(QUrl avatarPath READ avatarPath NOTIFY isAuthorizedChanged)
    Q_PROPERTY(QString displayName READ displayName NOTIFY isAuthorizedChanged)

public:
    explicit AccountInfoModel(QObject* parent = nullptr);

    Q_INVOKABLE void init();
    Q_INVOKABLE void signOut() const;
    Q_INVOKABLE void openAuthorizationDialog() const;

    bool isAuthorized() const;
    QUrl avatarPath() const;
    QString displayName() const;

signals:
    void isAuthorizedChanged();
};
}
