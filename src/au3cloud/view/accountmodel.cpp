/*
* Audacity: A Digital Audio Editor
*/
#include "accountmodel.h"

#include "framework/global/io/path.h"
#include "framework/global/types/retval.h"

using namespace au::au3cloud;

AccountModel::AccountModel(QObject* parent)
    : QObject(parent), muse::Contextable(muse::iocCtxForQmlObject(this))
{
}

void AccountModel::init()
{
    authorization()->authState().ch.onReceive(this, [this] (const auto&) {
        emit isAuthorizedChanged();
    });
}

bool AccountModel::isAuthorized() const
{
    return std::holds_alternative<au3cloud::Authorized>(authorization()->authState().val);
}

QUrl AccountModel::avatarPath() const
{
    return muse::io::path_t{ authorization()->accountInfo().avatarPath }.toQUrl();
}

QString AccountModel::displayName() const
{
    std::string displayName
        = !authorization()->accountInfo().displayName.empty()
          ? authorization()->accountInfo().displayName
          : authorization()->accountInfo().userSlug;
    return QString::fromStdString(displayName);
}

void AccountModel::signOut() const
{
    authorization()->signOut();
}

void AccountModel::openSignInDialog() const
{
    muse::actions::ActionQuery query("audacity://cloud/open-signin-dialog");
    query.addParam("sync", muse::Val(true));
    query.addParam("showTourPage", muse::Val(false));

    dispatcher()->dispatch(query);
}

void AccountModel::openCreateAccountDialog() const
{
    muse::actions::ActionQuery query("audacity://cloud/open-create-account-dialog");
    query.addParam("sync", muse::Val(true));
    query.addParam("showTourPage", muse::Val(false));

    dispatcher()->dispatch(query);
}
