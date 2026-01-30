/*
* Audacity: A Digital Audio Editor
*/

#include "signinaudiocompagemodel.h"

using namespace au::appshell;

SigninAudiocomPageModel::SigninAudiocomPageModel(QObject* parent)
    : QObject(parent), muse::Injectable(muse::iocCtxForQmlObject(this))
{
}

void SigninAudiocomPageModel::init()
{
    authorization()->authState().ch.onReceive(this, [this](au::au3cloud::AuthState newState) {
        if (newState == au::au3cloud::AuthState::NotAuthorized) {
            const auto& message = m_isRegistering
                                  ? qtrc("appshell/gettingstarted", "Registration failed. Please try again.")
                                  : qtrc("appshell/gettingstarted", "Incorrect email or password. Please try again.");
            setErrorMessage(message);
        } else {
            setErrorMessage(QString());
        }

        m_state = newState;

        emit authInProgressChanged();
        emit authorizedChanged();
    });
}

void SigninAudiocomPageModel::triggerAction(const QString& email, const QString& password)
{
    m_isRegistering
    ? authorization()->registerWithPassword(email.toStdString(), password.toStdString())
    : authorization()->signInWithPassword(email.toStdString(), password.toStdString());
}

void SigninAudiocomPageModel::signInWithSocial(const QString& provider)
{
    authorization()->signInWithSocial(provider.toStdString());
}

bool SigninAudiocomPageModel::authInProgress() const
{
    return m_state == au::au3cloud::AuthState::Authorizing;
}

bool SigninAudiocomPageModel::authorized() const
{
    return m_state == au::au3cloud::AuthState::Authorized;
}

bool SigninAudiocomPageModel::isRegistering() const
{
    return m_isRegistering;
}

void SigninAudiocomPageModel::setIsRegistering(bool isRegistering)
{
    if (m_isRegistering != isRegistering) {
        m_isRegistering = isRegistering;
        emit isRegisteringChanged();
    }
}

bool SigninAudiocomPageModel::showErrorMessage() const
{
    return !m_errorMessage.isEmpty();
}

QString SigninAudiocomPageModel::errorMessage() const
{
    return m_errorMessage;
}

void SigninAudiocomPageModel::setErrorMessage(const QString& errorMessage)
{
    if (m_errorMessage != errorMessage) {
        m_errorMessage = errorMessage;
        emit errorMessageChanged();
        emit showErrorMessageChanged();
    }
}
