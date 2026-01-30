/*
* Audacity: A Digital Audio Editor
*/
#include "framework/global/io/path.h"

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

QUrl CloudTestsModel::avatarPath() const
{
    muse::io::path_t filePath = userData()->getAvatarPath();
    return filePath.toQUrl();
}

QString CloudTestsModel::displayName() const
{
    return QString::fromStdString(userData()->getDisplayName());
}
