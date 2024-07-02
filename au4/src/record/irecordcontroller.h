/*
* Audacity: A Digital Audio Editor
*/
#ifndef AU_RECORD_IRECORDONTROLLER_H
#define AU_RECORD_IRECORDONTROLLER_H

#include "modularity/imoduleinterface.h"

#include "async/notification.h"

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
};
}

#endif // AU_RECORD_IRECORDONTROLLER_H
