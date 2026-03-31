/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "framework/global/async/asyncable.h"
#include "framework/global/modularity/ioc.h"
#include "framework/actions/iactionsdispatcher.h"

#include "context/iglobalcontext.h"
#include "context/iplaybackstate.h"
#include "trackedit/iprojecthistory.h"
#include "trackedit/itrackeditinteraction.h"
#include "trackedit/iselectioncontroller.h"
#include "audio/iaudioengine.h"
#include "au3wrap/au3types.h"

#include "../../irecord.h"
#include "irecordcontroller.h"

struct TransportSequences;
struct AudioIOStartStreamOptions;

namespace au::record {
class Au3Record : public IRecord, public muse::async::Asyncable, public muse::Contextable
{
    muse::GlobalInject<au::audio::IAudioEngine> audioEngine;

    muse::ContextInject<au::context::IGlobalContext> globalContext{ this };
    muse::ContextInject<muse::actions::IActionsDispatcher> dispatcher{ this };
    muse::ContextInject<au::trackedit::IProjectHistory> projectHistory{ this };
    muse::ContextInject<trackedit::ITrackeditInteraction> trackeditInteraction{ this };
    muse::ContextInject<au::trackedit::ISelectionController> selectionController{ this };

public:
    Au3Record(const muse::modularity::ContextPtr& ctx)
        : muse::Contextable(ctx) {}

    void init();

    muse::Ret start() override;
    muse::Ret pause() override;
    muse::Ret stop() override;

    IAudioInputPtr audioInput() const override;

    muse::secs_t recordPosition() const override;
    muse::async::Channel<muse::secs_t> recordPositionChanged() const override;

    muse::async::Notification recordingFinished() const override;

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
    muse::async::Notification m_recordingFinished;

    context::IPlaybackStatePtr playbackState() const;
};
}
