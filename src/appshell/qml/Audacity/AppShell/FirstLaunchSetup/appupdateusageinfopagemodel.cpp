/*
* Audacity: A Digital Audio Editor
*/

#include "appupdateusageinfopagemodel.h"

using namespace au::appshell;

AppUpdateUsageInfoPageModel::AppUpdateUsageInfoPageModel(QObject* parent)
    : QObject(parent), muse::Injectable(muse::iocCtxForQmlObject(this))
{
}

void AppUpdateUsageInfoPageModel::setSendAnonymousUsageInfo(bool allow)
{
    usageInfo()->setSendAnonymousUsageInfo(allow);
}
