/*
 * Audacity: A Digital Audio Editor
 */

#include "recordingpreferencesmodel.h"

using namespace au::appshell;

RecordingPreferencesModel::RecordingPreferencesModel(QObject* parent)
    : QObject(parent)
{
    recordConfiguration()->micMeteringChanged().onNotify(this, [this]() {
        emit micMeteringChanged();
    });
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

bool RecordingPreferencesModel::micMetering() const
{
    return recordConfiguration()->micMetering();
}

void RecordingPreferencesModel::setMicMetering(bool enabled)
{
    if (micMetering() != enabled) {
        recordConfiguration()->setMicMetering(enabled);
    }
}
