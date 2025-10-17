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

bool RecordingPreferencesModel::audibleInputMonitoring() const
{
    return record()->audioInput()->audibleInputMonitoring();
}

void RecordingPreferencesModel::setAudibleInputMonitoring(bool enabled)
{
    record()->audioInput()->setAudibleInputMonitoring(enabled);
    emit audibleInputMonitoringChanged();
}

bool RecordingPreferencesModel::isMicMeteringOn() const
{
    return recordConfiguration()->isMicMeteringOn();
}

void RecordingPreferencesModel::setIsMicMeteringOn(bool enabled)
{
    recordConfiguration()->setIsMicMeteringOn(enabled);
    emit isMicMeteringOnChanged();
}
