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

bool UsageInfoPreferencesModel::sendCrashReports() const
{
    return diagnosticsConfiguration()->isDumpUploadAllowed();
}

void UsageInfoPreferencesModel::setSendCrashReports(bool send)
{
    if (send == sendCrashReports()) {
        return;
    }

    diagnosticsConfiguration()->setIsDumpUploadAllowed(send);
    emit sendCrashReportsChanged(send);
}
