#pragma once

#include "playback/iplayback.h"

namespace au::au3 {
class Au3Player;
class Au3AudioOutput;
class Au3Playback : public playback::IPlayback
{
public:
    Au3Playback() = default;

    std::shared_ptr<playback::IPlayer> player(audio::TrackSequenceId id = -1) const override;

    std::shared_ptr<playback::IAudioOutput> audioOutput() const override;

private:
    mutable std::shared_ptr<Au3Player> m_player;
    mutable std::shared_ptr<Au3AudioOutput> m_audioOutput;
};
}
