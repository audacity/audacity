/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "global/async/asyncable.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "au3audio/iaudioengine.h"
#include "actions/iactionsdispatcher.h"
#include "trackedit/iprojecthistory.h"
#include "playback/iplayback.h"
#include "trackedit/itrackeditinteraction.h"
#include "trackedit/iselectioncontroller.h"

#include "au3wrap/au3types.h"

#include "../../irecord.h"
#include "irecordcontroller.h"

struct TransportSequences;
struct AudioIOStartStreamOptions;

namespace au::record {
class Au3Record : public IRecord, public muse::async::Asyncable
{
    muse::Inject<au::context::IGlobalContext> globalContext;
    muse::Inject<au::audio::IAudioEngine> audioEngine;
    muse::Inject<muse::actions::IActionsDispatcher> dispatcher;
    muse::Inject<au::trackedit::IProjectHistory> projectHistory;
    muse::Inject<au::playback::IPlayback> playback;
    muse::Inject<trackedit::ITrackeditInteraction> trackeditInteraction;
    muse::Inject<au::trackedit::ISelectionController> selectionController;

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
        trackedit::ClipKey clipKey;
        bool linkedToPendingClip;
    };

    au3::Au3Project& projectRef() const;

    bool canStopAudioStream() const;

    muse::Ret doRecord(au3::Au3Project& project, const TransportSequences& sequences, double t0, double t1, bool altAppearance,
                       const double audioStreamSampleRate);
    void cancelRecording();
    void commitRecording();

    void notifyAboutRecordClipsChanged();

    mutable muse::async::Channel<float> m_playbackVolumeChanged;

    IAudioInputPtr m_audioInput;
    std::vector<RecordData> m_recordData;

    muse::ValCh<muse::secs_t> m_recordPosition;
};
}
