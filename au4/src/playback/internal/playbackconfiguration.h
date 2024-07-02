/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../iplaybackconfiguration.h"

namespace au::playback {
class PlaybackConfiguration : public IPlaybackConfiguration
{
public:
    muse::draw::Color playColor() const override;
};
}
