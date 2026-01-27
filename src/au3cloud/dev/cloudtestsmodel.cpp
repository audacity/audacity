/*
* Audacity: A Digital Audio Editor
*/
#include "cloudtestsmodel.h"

using namespace au::au3cloud;

CloudTestsModel::CloudTestsModel(QObject* parent)
    : QObject(parent), muse::Injectable(muse::iocCtxForQmlObject(this))
{
}

void CloudTestsModel::init()
{
    authorization()->authState().ch.onReceive(this, [this] (const auto&) {
        emit isAuthorizedChanged();
    });
}

void CloudTestsModel::signOut()
{
    authorization()->signOut();
}

bool CloudTestsModel::isAuthorized() const
{
    return authorization()->authState().val == AuthState::Authorized;
}

std::string CloudTestsModel::avatarPath() const
{
    return userData()->getAvatarPath();
}

std::string CloudTestsModel::displayName() const
{
    return userData()->getDisplayName();
}
