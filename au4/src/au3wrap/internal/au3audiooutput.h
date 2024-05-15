/*
* Audacity: A Digital Audio Editor
*/

#ifndef AU_AU3WRAP_AU3AUDIOOUTPUT_H
#define AU_AU3WRAP_AU3AUDIOOUTPUT_H

#include "global/async/asyncable.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"

#include "../iau3audiooutput.h"

class AudacityProject;
class InOutMeter;

namespace au::au3 {
class Au3AudioOutput : public IAu3AudioOutput, public muse::async::Asyncable
{
    muse::Inject<au::context::IGlobalContext> globalContext;

public:
    Au3AudioOutput();

    muse::async::Promise<float> playbackVolume() const override;
    void setPlaybackVolume(float volume) override;
    muse::async::Channel<float> playbackVolumeChanged() const override;

    muse::async::Promise<muse::async::Channel<audio::audioch_t, audio::AudioSignalVal>> playbackSignalChanges() const override;

private:
    AudacityProject& projectRef() const;

    void initMeter();

    mutable muse::async::Channel<float> m_playbackVolumeChanged;

    std::shared_ptr<InOutMeter> m_outputMeter;
};
}

#endif // AU_AU3WRAP_AU3AUDIOOUTPUT_H
