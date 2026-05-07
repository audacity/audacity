/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/global/async/asyncable.h"

#include "modularity/ioc.h"
#include "au3cloud/iauthorization.h"
#include "framework/interactive/iinteractive.h"
#include "framework/actions/iactionsdispatcher.h"

namespace au::au3cloud {
class AccountModel : public QObject, public muse::async::Asyncable, public muse::Contextable
{
    Q_OBJECT

    muse::GlobalInject<au::au3cloud::IAuthorization> authorization;

    muse::ContextInject<muse::IInteractive> interactive { this };
    muse::ContextInject<muse::actions::IActionsDispatcher> dispatcher { this };

    Q_PROPERTY(bool isAuthorized READ isAuthorized NOTIFY isAuthorizedChanged)
    Q_PROPERTY(QUrl avatarPath READ avatarPath NOTIFY isAuthorizedChanged)
    Q_PROPERTY(QString displayName READ displayName NOTIFY isAuthorizedChanged)

public:
    explicit AccountModel(QObject* parent = nullptr);

    Q_INVOKABLE void init();
    Q_INVOKABLE void signOut() const;
    Q_INVOKABLE void openSignInDialog() const;
    Q_INVOKABLE void openCreateAccountDialog() const;

    bool isAuthorized() const;
    QUrl avatarPath() const;
    QString displayName() const;

signals:
    void isAuthorizedChanged();
};
}
