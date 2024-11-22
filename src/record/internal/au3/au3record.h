/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "global/async/asyncable.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "au3audio/iaudioengine.h"
#include "trackedit/iprojecthistory.h"

#include "au3wrap/au3types.h"

#include "../../irecord.h"

struct TransportSequences;
struct AudioIOStartStreamOptions;

namespace au::record {
class InOutMeter;
class Au3Record : public IRecord, public muse::async::Asyncable
{
    muse::Inject<au::context::IGlobalContext> globalContext;
    muse::Inject<au::audio::IAudioEngine> audioEngine;
    muse::Inject<au::trackedit::IProjectHistory> projectHistory;

public:
    void init();

    muse::Ret start() override;
    muse::Ret pause() override;
    muse::Ret stop() override;

    IAudioInputPtr audioInput() const override;

private:
    struct RecordData {
        std::vector<trackedit::TrackId> tracksIds;
        std::vector<trackedit::ClipKey> clipsKeys;

        void clear()
        {
            tracksIds.clear();
            clipsKeys.clear();
        }
    };

    au3::Au3Project& projectRef() const;

    bool canStopAudioStream() const;

    muse::Ret doRecord(au3::Au3Project& project, const TransportSequences& sequences, double t0, double t1, bool altAppearance,
                       const AudioIOStartStreamOptions& options);
    void cancelRecording();
    void commitRecording();

    void notifyAboutRecordClipsChanged();

    mutable muse::async::Channel<float> m_playbackVolumeChanged;

    IAudioInputPtr m_audioInput;
    RecordData m_recordData;
};
}
