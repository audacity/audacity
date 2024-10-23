#include "au3playback.h"

#include "au3player.h"
#include "au3audiooutput.h"

using namespace au::playback;

au::playback::IPlayerPtr Au3Playback::player(TrackSequenceId) const
{
    if (!m_player) {
        m_player = std::make_shared<Au3Player>();
    }
    return m_player;
}

std::shared_ptr<au::playback::IAudioOutput> Au3Playback::audioOutput() const
{
    if (!m_audioOutput) {
        m_audioOutput = std::make_shared<Au3AudioOutput>();
    }
    return m_audioOutput;
}
