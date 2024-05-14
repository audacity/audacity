/*
* Audacity: A Digital Audio Editor
*/

#ifndef AU_AU3WRAP_AU3AUDIOOUTPUT_H
#define AU_AU3WRAP_AU3AUDIOOUTPUT_H

#include "global/async/asyncable.h"

#include "../iau3audiooutput.h"

namespace au::au3 {
class Au3AudioOutput : public IAu3AudioOutput, public muse::async::Asyncable
{
public:
    muse::async::Promise<float> playbackVolume() const override;
    void setPlaybackVolume(float volume) override;
    muse::async::Channel<float> playbackVolumeChanged() const override;

    muse::async::Promise<au::audio::AudioSignalChanges> playbackSignalChanges() const override;

private:
    mutable muse::async::Channel<float> m_playbackVolumeChanged;
};
}

#endif // AU_AU3WRAP_AU3AUDIOOUTPUT_H
