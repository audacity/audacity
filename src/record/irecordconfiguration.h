/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <string>

#include "global/modularity/imoduleinterface.h"
#include "global/async/notification.h"
#include "draw/types/color.h"
#include "record/timedrecordingtypes.h"

namespace au::record {
//! Naming scheme for newly recorded tracks
struct RecordingTrackNameOptions {
    bool useCustomName = false;
    std::string customName;
    bool addTrackNumber = false;
    bool addDateStamp = false;
    bool addTimeStamp = false;
};

class IRecordConfiguration : MODULE_GLOBAL_INTERFACE
{
    INTERFACE_ID(IRecordConfiguration)

public:
    virtual ~IRecordConfiguration() = default;

    virtual bool isMicMeteringOn() const = 0;
    virtual void setIsMicMeteringOn(bool enable) = 0;
    virtual muse::async::Notification isMicMeteringOnChanged() const = 0;

    virtual bool isInputMonitoringOn() const = 0;
    virtual void setIsInputMonitoringOn(bool enable) = 0;
    virtual muse::async::Notification isInputMonitoringOnChanged() const = 0;

    virtual double leadInTimeDuration() const = 0;
    virtual void setLeadInTimeDuration(double seconds) = 0;
    virtual muse::async::Notification leadInTimeDurationChanged() const = 0;

    virtual double crossfadeDuration() const = 0;
    virtual void setCrossfadeDuration(double milliseconds) = 0;
    virtual muse::async::Notification crossfadeDurationChanged() const = 0;

    virtual RecordingTrackNameOptions recordingTrackNameOptions() const = 0;

    virtual TimedRecordingOptions timedRecordingOptions() const = 0;
    virtual void setTimedRecordingOptions(const TimedRecordingOptions& options) = 0;
};
}
