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
}

bool RecordingPreferencesModel::isInputMonitoringOn() const
{
    return record()->audioInput()->isInputMonitoringOn();
}

void RecordingPreferencesModel::setIsInputMonitoringOn(bool enabled)
{
    record()->audioInput()->setIsInputMonitoringOn(enabled);
    emit isInputMonitoringOnChanged();
}

bool RecordingPreferencesModel::isMicMeteringOn() const
{
    return recordConfiguration()->isMicMeteringOn();
}

void RecordingPreferencesModel::setIsMicMeteringOn(bool enabled)
{
    recordConfiguration()->setIsMicMeteringOn(enabled);
}
