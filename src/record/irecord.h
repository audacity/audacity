/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include <memory>
#include <vector>

#include "async/notification.h"
#include "global/types/ret.h"

#include "modularity/imoduleinterface.h"

#include "trackedit/trackedittypes.h"
#include "iaudioinput.h"

namespace au::record {
class IRecord : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IRecord)
public:
    virtual ~IRecord() = default;

    virtual muse::Ret start() = 0;
    virtual muse::Ret pause() = 0;
    virtual muse::Ret stop() = 0;
    virtual muse::Ret leadInRecording() = 0;

    virtual IAudioInputPtr audioInput() const = 0;

    virtual muse::async::Channel<muse::secs_t> recordPositionChanged() const = 0;

    virtual const std::vector<trackedit::ClipKey>& recordingClipKeys() const = 0;

    virtual muse::async::Notification recordingFinished() const = 0;
};

using IAudioRecordPtr = std::shared_ptr<IRecord>;
}
