/*
 * Audacity: A Digital Audio Editor
 */

#include "recordingpreferencesmodel.h"

using namespace au::appshell;

RecordingPreferencesModel::RecordingPreferencesModel(QObject* parent)
    : QObject(parent)
{
}

bool RecordingPreferencesModel::audibleInputMonitoring() const
{
    return record()->audioInput()->audibleInputMonitoring();
}

void RecordingPreferencesModel::setAudibleInputMonitoring(bool enabled)
{
    if (audibleInputMonitoring() != enabled) {
        record()->audioInput()->setAudibleInputMonitoring(enabled);
        emit audibleInputMonitoringChanged();
    }
}
