/*
* Audacity: A Digital Audio Editor
*/

#ifndef AU_AU3WRAP_AU3AUDIOINPUT_H
#define AU_AU3WRAP_AU3AUDIOINPUT_H

#include "global/async/asyncable.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"

#include "../iau3audioinput.h"

class AudacityProject;

namespace au::au3 {
class InOutMeter;
class Au3AudioInput : public IAu3AudioInput, public muse::async::Asyncable
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

    std::shared_ptr<InOutMeter> m_inputMeter;
};
}

#endif // AU_AU3WRAP_AU3AUDIOINPUT_H
