/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "global/async/asyncable.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"

#include "au3wrap/au3types.h"
#include "au3wrap/internal/au3audiometer.h"

#include "../../iaudioinput.h"
namespace au::record {
class Au3AudioInput : public IAudioInput, public muse::async::Asyncable
{
    muse::Inject<au::context::IGlobalContext> globalContext;

public:
    Au3AudioInput();

    muse::async::Promise<float> recordVolume() const override;
    void setRecordVolume(float volume) override;
    muse::async::Channel<float> recordVolumeChanged() const override;

    muse::async::Channel<audio::audioch_t, audio::MeterSignal> recordSignalChanges() const override;
    muse::async::Channel<au::audio::audioch_t, au::audio::MeterSignal> recordTrackSignalChanges(int64_t key) const;

private:
    au3::Au3Project& projectRef() const;

    void initMeter();

    mutable muse::async::Channel<float> m_recordVolumeChanged;

    std::shared_ptr<au::au3::Meter> m_inputMeter;
};
}
