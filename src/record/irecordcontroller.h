/*
* Audacity: A Digital Audio Editor
*/
#ifndef AU_RECORD_IRECORDONTROLLER_H
#define AU_RECORD_IRECORDONTROLLER_H

#include "framework/global/modularity/imoduleinterface.h"
#include "framework/global/async/notification.h"
#include "framework/global/async/channel.h"
#include "framework/global/types/secs.h"

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

    virtual muse::secs_t recordPosition() const = 0;
    virtual muse::async::Channel<muse::secs_t> recordPositionChanged() const = 0;

    virtual bool isMicMeteringOn() const = 0;
    virtual muse::async::Notification isMicMeteringOnChanged() const = 0;

    virtual bool isInputMonitoringOn() const = 0;
    virtual muse::async::Notification isInputMonitoringOnChanged() const = 0;
};
}

#endif // AU_RECORD_IRECORDONTROLLER_H
