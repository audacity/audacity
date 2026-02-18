/*
* Audacity: A Digital Audio Editor
*/
#include "accountinfomodel.h"

#include "framework/global/io/path.h"
#include "framework/global/types/retval.h"

using namespace au::au3cloud;

AccountInfoModel::AccountInfoModel(QObject* parent)
    : QObject(parent), muse::Injectable(muse::iocCtxForQmlObject(this))
{
}

void AccountInfoModel::init()
{
    authorization()->authState().ch.onReceive(this, [this] (const auto&) {
        emit isAuthorizedChanged();
    });
}

bool AccountInfoModel::isAuthorized() const
{
    return std::holds_alternative<au3cloud::Authorized>(authorization()->authState().val);
}

QUrl AccountInfoModel::avatarPath() const
{
    return muse::io::path_t{ authorization()->accountInfo().avatarPath }.toQUrl();
}

QString AccountInfoModel::displayName() const
{
    return QString::fromStdString(authorization()->accountInfo().displayName);
}

void AccountInfoModel::signOut() const
{
    authorization()->signOut();
}

void AccountInfoModel::openAuthorizationDialog() const
{
    interactive()->openSync("audacity://signin/audiocom");
}
