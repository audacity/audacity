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
    recordConfiguration()->preRollDurationChanged().onNotify(this, [this]() {
        emit preRollDurationChanged();
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

double RecordingPreferencesModel::preRollDuration() const
{
    return recordConfiguration()->preRollDuration();
}

void RecordingPreferencesModel::setPreRollDuration(double seconds)
{
    if (preRollDuration() == seconds) {
        return;
    }
    recordConfiguration()->setPreRollDuration(seconds);
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
