/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "global/async/asyncable.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"

#include "../../iaudioinput.h"

class AudacityProject;

namespace au::au3 {
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

    muse::async::Promise<muse::async::Channel<audio::audioch_t, audio::AudioSignalVal>> recordSignalChanges() const override;

private:
    AudacityProject& projectRef() const;

    void initMeter();

    mutable muse::async::Channel<float> m_recordVolumeChanged;

    std::shared_ptr<au3::InOutMeter> m_inputMeter;
};
}
