/*
 * Audacity: A Digital Audio Editor
 */

#include "recordingpreferencesmodel.h"

using namespace au::appshell;

RecordingPreferencesModel::RecordingPreferencesModel(QObject* parent)
    : QObject(parent)
{
    recordConfiguration()->isMicMeteringOnChanged().onNotify(this, [this]() {
        emit isMicMeteringOnChanged();
    });
    recordConfiguration()->isInputMonitoringOnChanged().onNotify(this, [this]() {
        emit isInputMonitoringOnChanged();
    });
}

bool RecordingPreferencesModel::isInputMonitoringOn() const
{
    return recordConfiguration()->isInputMonitoringOn();
}

void RecordingPreferencesModel::setIsInputMonitoringOn(bool enabled)
{
    recordConfiguration()->setIsInputMonitoringOn(enabled);
}

bool RecordingPreferencesModel::isMicMeteringOn() const
{
    return recordConfiguration()->isMicMeteringOn();
}

void RecordingPreferencesModel::setIsMicMeteringOn(bool enabled)
{
    recordConfiguration()->setIsMicMeteringOn(enabled);
}
