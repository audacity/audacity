/*
 * Audacity: A Digital Audio Editor
 */
#include "usageinfopreferencesmodel.h"

using namespace au::appshell;

UsageInfoPreferencesModel::UsageInfoPreferencesModel(QObject* parent)
    : QObject(parent)
{
}

bool UsageInfoPreferencesModel::sendAnonymousUsageInfo() const
{
    return usageInfo()->getSendAnonymousUsageInfo();
}

void UsageInfoPreferencesModel::setSendAnonymousUsageInfo(bool allow)
{
    if (allow == sendAnonymousUsageInfo()) {
        return;
    }

    usageInfo()->setSendAnonymousUsageInfo(allow);
    emit sendAnonymousUsageInfoChanged(allow);
}
