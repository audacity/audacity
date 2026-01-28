/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>
#include <QtQml/qqmlregistration.h>

#include "framework/global/async/asyncable.h"

#include "framework/global/modularity/ioc.h"
#include "au3cloud/iauthorization.h"

namespace au::appshell {
class SigninAudiocomPageModel : public QObject, public muse::async::Asyncable, public muse::Injectable
{
    Q_OBJECT
    QML_ELEMENT

    muse::Inject<au::au3cloud::IAuthorization> authorization { this };

    Q_PROPERTY(bool authInProgress READ authInProgress NOTIFY authInProgressChanged)
    Q_PROPERTY(bool authFailed READ authFailed NOTIFY authFailedChanged)
    Q_PROPERTY(bool authSucceeded READ authSucceeded NOTIFY authInProgressChanged)

public:
    explicit SigninAudiocomPageModel(QObject* parent = nullptr);

    Q_INVOKABLE void init();
    Q_INVOKABLE void signIn(const QString& email, const QString& password);
    Q_INVOKABLE void signInWithSocial(const QString& provider);
    Q_INVOKABLE void signOut();

    bool authInProgress() const;
    bool authFailed() const;
    bool authSucceeded() const;

signals:
    void authInProgressChanged();
    void authFailedChanged();
    void authSucceededChanged();

private:
    au::au3cloud::AuthState m_state;
    bool m_authFailed = false;
    bool m_authSucceeded = false;
};
}
