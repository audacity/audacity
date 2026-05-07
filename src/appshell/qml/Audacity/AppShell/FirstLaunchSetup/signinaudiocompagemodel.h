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
class SigninAudiocomPageModel : public QObject, public muse::async::Asyncable, public muse::Contextable
{
    Q_OBJECT
    QML_ELEMENT

    muse::GlobalInject<au::au3cloud::IAuthorization> authorization;

    Q_PROPERTY(bool authInProgress READ authInProgress NOTIFY authInProgressChanged)
    Q_PROPERTY(bool authorized READ authorized NOTIFY authorizedChanged)

    Q_PROPERTY(bool showErrorMessage READ showErrorMessage NOTIFY showErrorMessageChanged)
    Q_PROPERTY(QString errorMessage READ errorMessage NOTIFY errorMessageChanged)

public:
    explicit SigninAudiocomPageModel(QObject* parent = nullptr);

    Q_INVOKABLE void init();
    Q_INVOKABLE void signInWithSocial(const QString& provider);
    Q_INVOKABLE void signInWithEmail(const QString& email, const QString& password);
    Q_INVOKABLE void signUpWithEmail(const QString& email, const QString& password);

    bool authInProgress() const;
    bool authorized() const;

    bool showErrorMessage() const;
    QString errorMessage() const;

signals:
    void authInProgressChanged();
    void authorizedChanged();

    void showErrorMessageChanged();
    void errorMessageChanged();

private:
    void setErrorMessage(const QString& message);

    au::au3cloud::AuthState m_state = au::au3cloud::AuthState(au::au3cloud::NotAuthorized());
    QString m_errorMessage;
};
}
