/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "global/async/notification.h"
#include "modularity/imoduleinterface.h"

#include "playbacktypes.h"

namespace au::playback {
class IPlaybackUiState : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IPlaybackUiState)

public:
    virtual ~IPlaybackUiState() = default;

    virtual PlaybackMeterPosition::MeterPosition playbackMeterPosition() const = 0;
    virtual void setPlaybackMeterPosition(PlaybackMeterPosition::MeterPosition position) = 0;
    virtual muse::async::Notification playbackMeterPositionChanged() const = 0;
};
}
