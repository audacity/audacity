/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>

#include "framework/global/async/asyncable.h"

#include "framework/global/modularity/ioc.h"
#include "au3cloud/iuserdata.h"
#include "au3cloud/iauthorization.h"

namespace au::au3cloud {
class CloudTestsModel : public QObject, public muse::async::Asyncable, public muse::Injectable
{
    Q_OBJECT

    muse::Inject<IAuthorization> authorization = { this };
    muse::Inject<IUserData> userData = { this };

    Q_PROPERTY(bool isAuthorized READ isAuthorized NOTIFY isAuthorizedChanged)
    Q_PROPERTY(std::string avatarPath READ avatarPath NOTIFY avatarPathChanged)
    Q_PROPERTY(std::string displayName READ displayName NOTIFY displayNameChanged)

public:
    explicit CloudTestsModel(QObject* parent = nullptr);

    Q_INVOKABLE void init();
    Q_INVOKABLE void signOut();

    bool isAuthorized() const;
    std::string avatarPath() const;
    std::string displayName() const;

signals:
    void displayNameChanged();
    void avatarPathChanged();
    void isAuthorizedChanged();
};
}
