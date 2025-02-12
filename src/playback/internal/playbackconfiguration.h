/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/ioc.h"
#include "ui/iuiconfiguration.h"

#include "../iplaybackconfiguration.h"

namespace au::playback {
class PlaybackConfiguration : public IPlaybackConfiguration
{
    muse::Inject<muse::ui::IUiConfiguration> uiConfiguration;

public:
    muse::draw::Color playColor() const override;

    TimecodeFormatType playbackTimeItemFormat() const override;
    void setPlaybackTimeItemFormat(TimecodeFormatType format) override;
    muse::async::Notification playbackTimeItemFormatChanged() const override;
};
}
