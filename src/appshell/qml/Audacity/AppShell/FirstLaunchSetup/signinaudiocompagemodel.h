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
    Q_PROPERTY(bool authorized READ authorized NOTIFY authorizedChanged)
    Q_PROPERTY(bool isRegistering READ isRegistering WRITE setIsRegistering NOTIFY isRegisteringChanged)

    Q_PROPERTY(bool showErrorMessage READ showErrorMessage NOTIFY showErrorMessageChanged)
    Q_PROPERTY(QString errorMessage READ errorMessage NOTIFY errorMessageChanged)

public:
    explicit SigninAudiocomPageModel(QObject* parent = nullptr);

    Q_INVOKABLE void init();
    Q_INVOKABLE void signInWithSocial(const QString& provider);
    Q_INVOKABLE void triggerAction(const QString& email, const QString& password);

    bool authInProgress() const;
    bool authorized() const;
    bool isRegistering() const;

    bool showErrorMessage() const;
    QString errorMessage() const;

signals:
    void authInProgressChanged();
    void authorizedChanged();
    void isRegisteringChanged();

    void showErrorMessageChanged();
    void errorMessageChanged();
public slots:
    void setIsRegistering(bool isRegistering);

private:
    void setErrorMessage(const QString& message);

    au::au3cloud::AuthState m_state;
    bool m_isRegistering = false;

    QString m_errorMessage;
};
}
