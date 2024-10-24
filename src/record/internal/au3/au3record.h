/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "global/async/asyncable.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"

#include "au3wrap/au3types.h"

#include "../../irecord.h"

class TransportSequences;
struct AudioIOStartStreamOptions;

namespace au::record {
class InOutMeter;
class Au3Record : public IRecord, public muse::async::Asyncable
{
    muse::Inject<au::context::IGlobalContext> globalContext;

public:
    void init();

    muse::Ret start() override;
    muse::Ret pause() override;
    muse::Ret stop() override;

    IAudioInputPtr audioInput() const override;

private:
    au3::Au3Project& projectRef() const;

    muse::Ret doRecord(au3::Au3Project& project, const TransportSequences& sequences, double t0, double t1, bool altAppearance,
                       const AudioIOStartStreamOptions& options);
    void cancelRecording();
    bool canStopAudioStream() const;

    mutable muse::async::Channel<float> m_playbackVolumeChanged;

    IAudioInputPtr m_audioInput;
};
}
