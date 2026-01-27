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
        if (m_state == au::au3cloud::AuthState::Authorizing
            && newState == au::au3cloud::AuthState::NotAuthorized) {
            m_authSucceeded = false;
            m_authFailed = true;
            emit authFailedChanged();
        } else if (newState == au::au3cloud::AuthState::Authorized
                   && m_state == au::au3cloud::AuthState::Authorizing) {
            m_authFailed = false;
            m_authSucceeded = true;
        } else {
            m_authFailed = false;
            m_authSucceeded = false;
        }
        m_state = newState;
        emit authInProgressChanged();
        emit authFailedChanged();
        emit authSucceededChanged();
    });
}

void SigninAudiocomPageModel::signIn(const QString& email, const QString& password)
{
    authorization()->signInWithPassword(email.toStdString(), password.toStdString());
}

void SigninAudiocomPageModel::signOut()
{
    authorization()->signOut();
}

bool SigninAudiocomPageModel::authInProgress() const
{
    return m_state == au::au3cloud::AuthState::Authorizing;
}

bool SigninAudiocomPageModel::authFailed() const
{
    return m_authFailed;
}

bool SigninAudiocomPageModel::authSucceeded() const
{
    return m_state == au::au3cloud::AuthState::Authorized;
}
