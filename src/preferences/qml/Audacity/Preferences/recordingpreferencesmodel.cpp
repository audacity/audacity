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
    recordConfiguration()->leadInTimeDurationChanged().onNotify(this, [this]() {
        emit leadInTimeDurationChanged();
    });
    recordConfiguration()->crossfadeDurationChanged().onNotify(this, [this]() {
        emit crossfadeDurationChanged();
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

double RecordingPreferencesModel::leadInTimeDuration() const
{
    return recordConfiguration()->leadInTimeDuration();
}

void RecordingPreferencesModel::setLeadInTimeDuration(double seconds)
{
    if (leadInTimeDuration() == seconds) {
        return;
    }
    recordConfiguration()->setLeadInTimeDuration(seconds);
}

double RecordingPreferencesModel::crossfadeDuration() const
{
    return recordConfiguration()->crossfadeDuration();
}

void RecordingPreferencesModel::setCrossfadeDuration(double milliseconds)
{
    if (crossfadeDuration() == milliseconds) {
        return;
    }
    recordConfiguration()->setCrossfadeDuration(milliseconds);
}
