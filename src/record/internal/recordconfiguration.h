/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/global/modularity/ioc.h"
#include "framework/ui/iuiconfiguration.h"

#include "../irecordconfiguration.h"

namespace au::record {
class RecordConfiguration : public IRecordConfiguration
{
    muse::GlobalInject<muse::ui::IUiConfiguration> uiConfiguration;

public:
    void init();

    bool isMicMeteringOn() const override;
    void setIsMicMeteringOn(bool enable) override;
    muse::async::Notification isMicMeteringOnChanged() const override;

    bool isInputMonitoringOn() const override;
    void setIsInputMonitoringOn(bool enable) override;
    muse::async::Notification isInputMonitoringOnChanged() const override;

    double leadInTimeDuration() const override;
    void setLeadInTimeDuration(double seconds) override;
    muse::async::Notification leadInTimeDurationChanged() const override;

    double crossfadeDuration() const override;
    void setCrossfadeDuration(double milliseconds) override;
    muse::async::Notification crossfadeDurationChanged() const override;

    bool isSoundActivatedRecordingOn() const override;
    void setIsSoundActivatedRecordingOn(bool enable) override;
    muse::async::Notification isSoundActivatedRecordingOnChanged() const override;

    double soundActivationLevel() const override;
    void setSoundActivationLevel(double db) override;
    muse::async::Notification soundActivationLevelChanged() const override;

    RecordingTrackNameOptions recordingTrackNameOptions() const override;

    TimedRecordingOptions timedRecordingOptions() const override;
    void setTimedRecordingOptions(const TimedRecordingOptions& options) override;

private:
    muse::async::Notification m_isMicMeteringOnChanged;
    muse::async::Notification m_isInputMonitoringOnChanged;
    muse::async::Notification m_leadInTimeDurationChanged;
    muse::async::Notification m_crossfadeDurationChanged;
    muse::async::Notification m_isSoundActivatedRecordingOnChanged;
    muse::async::Notification m_soundActivationLevelChanged;
};
}
