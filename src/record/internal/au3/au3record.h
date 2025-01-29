/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "modularity/ioc.h"

#include "actions/iactionsdispatcher.h"
#include "context/iglobalcontext.h"
#include "global/async/asyncable.h"

#include "au3audio/iaudioengine.h"
#include "au3wrap/au3types.h"
#include "trackedit/iprojecthistory.h"

#include "../../irecord.h"
#include "irecordcontroller.h"

struct TransportSequences;
struct AudioIOStartStreamOptions;

namespace au::record {
class InOutMeter;
class Au3Record : public IRecord, public muse::async::Asyncable
{
    muse::Inject<au::context::IGlobalContext> globalContext;
    muse::Inject<au::audio::IAudioEngine> audioEngine;
    muse::Inject<muse::actions::IActionsDispatcher> dispatcher;
    muse::Inject<au::trackedit::IProjectHistory> projectHistory;

public:
    void init();

    muse::Ret start() override;
    muse::Ret pause() override;
    muse::Ret stop() override;

    IAudioInputPtr audioInput() const override;

    muse::secs_t recordPosition() const override;
    muse::async::Channel<muse::secs_t> recordPositionChanged() const override;

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

    muse::ValCh<muse::secs_t> m_recordPosition;
};
}
