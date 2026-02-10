/*
* Audacity: A Digital Audio Editor
*/

#include "signinaudiocompagemodel.h"
#include <variant>

using namespace au::appshell;

SigninAudiocomPageModel::SigninAudiocomPageModel(QObject* parent)
    : QObject(parent), muse::Injectable(muse::iocCtxForQmlObject(this))
{
}

void SigninAudiocomPageModel::init()
{
    authorization()->signOut();
    authorization()->authState().ch.onReceive(this, [this](au::au3cloud::AuthState newState) {
        if (std::holds_alternative<au3cloud::NotAuthorized>(newState)) {
            const std::string& error = std::get<au3cloud::NotAuthorized>(newState).error;
            setErrorMessage(QString::fromStdString(error));
        } else {
            setErrorMessage(QString());
        }

        m_state = newState;

        emit authInProgressChanged();
        emit authorizedChanged();
    });
}

void SigninAudiocomPageModel::signInWithEmail(const QString& email, const QString& password)
{
    authorization()->signInWithPassword(email.toStdString(), password.toStdString());
}

void SigninAudiocomPageModel::signUpWithEmail(const QString& email, const QString& password)
{
    authorization()->registerWithPassword(email.toStdString(), password.toStdString());
}

void SigninAudiocomPageModel::signInWithSocial(const QString& provider)
{
    authorization()->signInWithSocial(provider.toStdString());
}

bool SigninAudiocomPageModel::authInProgress() const
{
    return std::holds_alternative<au3cloud::Authorizing>(m_state);
}

bool SigninAudiocomPageModel::authorized() const
{
    return std::holds_alternative<au3cloud::Authorized>(m_state);
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
