/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "global/async/asyncable.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"

#include "au3wrap/au3types.h"

#include "../../iaudioinput.h"

namespace au::playback {
class InOutMeter;
}

namespace au::record {
class Au3AudioInput : public IAudioInput, public muse::async::Asyncable
{
    muse::Inject<au::context::IGlobalContext> globalContext;

public:
    Au3AudioInput();

    muse::async::Promise<float> recordVolume() const override;
    void setRecordVolume(float volume) override;
    muse::async::Channel<float> recordVolumeChanged() const override;

    muse::async::Promise<muse::async::Channel<audio::audioch_t, audio::AudioSignalVal> > recordSignalChanges() const override;

private:
    au3::Au3Project& projectRef() const;

    void initMeter();

    mutable muse::async::Channel<float> m_recordVolumeChanged;

    std::shared_ptr<playback::InOutMeter> m_inputMeter;
};
}
