/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "async/notification.h"
#include "draw/types/color.h"

#include "playbacktypes.h"

#include "modularity/imoduleinterface.h"

namespace au::playback {
class IPlaybackConfiguration : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IPlaybackConfiguration)

public:
    virtual ~IPlaybackConfiguration() = default;

    virtual muse::draw::Color playColor() const = 0;

    virtual TimecodeFormatType playbackTimeItemFormat() const = 0;
    virtual void setPlaybackTimeItemFormat(TimecodeFormatType format) = 0;
    virtual muse::async::Notification playbackTimeItemFormatChanged() const = 0;
};
}
