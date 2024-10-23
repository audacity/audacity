/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../../iplayback.h"

namespace au::playback {
class Au3Player;
class Au3AudioOutput;
class Au3Playback : public IPlayback
{
public:
    Au3Playback() = default;

    std::shared_ptr<playback::IPlayer> player(TrackSequenceId id = -1) const override;

    std::shared_ptr<playback::IAudioOutput> audioOutput() const override;

private:
    mutable std::shared_ptr<Au3Player> m_player;
    mutable std::shared_ptr<Au3AudioOutput> m_audioOutput;
};
}
