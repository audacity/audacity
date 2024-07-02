/*
* Audacity: A Digital Audio Editor
*/

#ifndef AU_AU3WRAP_AU3RECORD_H
#define AU_AU3WRAP_AU3RECORD_H

#include "global/async/asyncable.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"

#include "../iau3record.h"

class AudacityProject;
class TransportSequences;
struct AudioIOStartStreamOptions;

namespace au::au3 {
class InOutMeter;
class Au3Record : public IAu3Record, public muse::async::Asyncable
{
    muse::Inject<au::context::IGlobalContext> globalContext;

public:
    void init();

    void start() override;
    void pause() override;
    void stop() override;

    IAu3AudioInputPtr audioInput() const override;

private:
    AudacityProject& projectRef() const;

    bool doRecord(AudacityProject& project, const TransportSequences& sequences, double t0, double t1, bool altAppearance,
                  const AudioIOStartStreamOptions& options);
    void cancelRecording();
    bool canStopAudioStream() const;

    mutable muse::async::Channel<float> m_playbackVolumeChanged;

    IAu3AudioInputPtr m_audioInput;
};
}

#endif // AU_AU3WRAP_AU3RECORD_H
