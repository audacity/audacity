/*
* Audacity: A Digital Audio Editor
*/

#ifdef AU_BUILD_CLOUD_AUDIOCOM

#include "appupdateusageinfopagemodel.h"

using namespace au::appshell;

AppUpdateUsageInfoPageModel::AppUpdateUsageInfoPageModel(QObject* parent)
    : QObject(parent), muse::Contextable(muse::iocCtxForQmlObject(this))
{
}

void AppUpdateUsageInfoPageModel::setSendAnonymousUsageInfo(bool allow)
{
    usageInfo()->setSendAnonymousUsageInfo(allow);
}

#endif // AU_BUILD_CLOUD_AUDIOCOM
