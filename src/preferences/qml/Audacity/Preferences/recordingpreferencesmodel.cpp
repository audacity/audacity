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
    recordConfiguration()->isOverdubOnChanged().onNotify(this, [this]() {
        emit isOverdubOnChanged();
    });
    recordConfiguration()->recordOnNewTrackChanged().onNotify(this, [this]() {
        emit recordOnNewTrackChanged();
    });
    recordConfiguration()->detectDropoutsChanged().onNotify(this, [this]() {
        emit detectDropoutsChanged();
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

bool RecordingPreferencesModel::isOverdubOn() const
{
    return recordConfiguration()->isOverdubOn();
}

void RecordingPreferencesModel::setIsOverdubOn(bool enabled)
{
    recordConfiguration()->setIsOverdubOn(enabled);
}

bool RecordingPreferencesModel::recordOnNewTrack() const
{
    return recordConfiguration()->recordOnNewTrack();
}

void RecordingPreferencesModel::setRecordOnNewTrack(bool enabled)
{
    recordConfiguration()->setRecordOnNewTrack(enabled);
}

bool RecordingPreferencesModel::detectDropouts() const
{
    return recordConfiguration()->detectDropouts();
}

void RecordingPreferencesModel::setDetectDropouts(bool enabled)
{
    recordConfiguration()->setDetectDropouts(enabled);
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
