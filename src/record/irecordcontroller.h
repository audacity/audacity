/*
* Audacity: A Digital Audio Editor
*/
#ifndef AU_RECORD_IRECORDONTROLLER_H
#define AU_RECORD_IRECORDONTROLLER_H

#include <vector>

#include "framework/global/modularity/imoduleinterface.h"
#include "framework/global/async/notification.h"
#include "framework/global/async/channel.h"
#include "framework/global/types/secs.h"
#include "trackedit/trackedittypes.h"

namespace au::record {
class IRecordController : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IRecordController)

public:
    virtual ~IRecordController() = default;

    virtual bool isRecordAllowed() const = 0;
    virtual muse::async::Notification isRecordAllowedChanged() const = 0;

    virtual bool isRecording() const = 0;
    virtual muse::async::Notification isRecordingChanged() const = 0;

    virtual const std::vector<trackedit::ClipKey>& recordingClipKeys() const = 0;

    virtual bool isMicMeteringOn() const = 0;
    virtual muse::async::Notification isMicMeteringOnChanged() const = 0;

    virtual bool isInputMonitoringOn() const = 0;
    virtual muse::async::Notification isInputMonitoringOnChanged() const = 0;

    virtual bool isLeadInRecording() const = 0;
    virtual muse::async::Notification isLeadInRecordingChanged() const = 0;
    virtual muse::secs_t leadInRecordingStartTime() const = 0;
    virtual std::vector<trackedit::TrackId> leadInRecordingTrackIds() const = 0;
};
}

#endif // AU_RECORD_IRECORDONTROLLER_H
