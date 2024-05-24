/*
* Audacity: A Digital Audio Editor
*/
#ifndef MU_PLAYBACK_PLAYBACKCONTROLLER_H
#define MU_PLAYBACK_PLAYBACKCONTROLLER_H

#include "modularity/ioc.h"
#include "async/asyncable.h"
#include "actions/iactionsdispatcher.h"
#include "actions/actionable.h"
#include "context/iglobalcontext.h"
#include "iinteractive.h"

#include "au3wrap/iau3playback.h"

#include "../iplaybackcontroller.h"

namespace au::playback {
class PlaybackController : public IPlaybackController, public muse::actions::Actionable, public muse::async::Asyncable
{
    INJECT_STATIC(muse::actions::IActionsDispatcher, dispatcher)
    INJECT_STATIC(au::context::IGlobalContext, globalContext)
    INJECT_STATIC(muse::IInteractive, interactive)
    INJECT_STATIC(au3::IAu3Playback, au3Playback)

public:
    void init();
    void deinit();

    bool isPlayAllowed() const override;
    muse::async::Notification isPlayAllowedChanged() const override;

    bool isPlaying() const override;
    muse::async::Notification isPlayingChanged() const override;

    void seek(const audio::msecs_t msecs) override;
    void reset() override;

    muse::async::Notification playbackPositionChanged() const override;
    muse::async::Channel<uint32_t> midiTickPlayed() const override;
    float playbackPositionInSeconds() const override;

    muse::async::Channel<audio::TrackId> trackAdded() const override;
    muse::async::Channel<audio::TrackId> trackRemoved() const override;

    // ISoloMuteState::SoloMuteState trackSoloMuteState(const TrackId& trackId) const override;
    // void setTrackSoloMuteState(const TrackId& trackId,
    //                            const ISoloMuteState::SoloMuteState& state) const override;

    bool actionChecked(const muse::actions::ActionCode& actionCode) const override;
    muse::async::Channel<muse::actions::ActionCode> actionCheckedChanged() const override;

    QTime totalPlayTime() const override;
    muse::async::Notification totalPlayTimeChanged() const override;

    muse::Progress loadingProgress() const override;

    bool canReceiveAction(const muse::actions::ActionCode& code) const override;

private:
    bool isPaused() const;
    bool isLoaded() const;

    bool isLoopEnabled() const;
    bool loopBoundariesSet() const;

    void onProjectChanged();

    void onSelectionChanged();
    void seekListSelection();
    void seekRangeSelection();

    void togglePlay();
    void rewindToStart(const muse::actions::ActionData& args);
    void play();
    void pause();
    void stop();
    void resume();

    audio::msecs_t playbackStartMsecs() const;
    audio::msecs_t playbackEndMsecs() const;

    void setCurrentPlaybackStatus(audio::PlaybackStatus status);

    void togglePlayRepeats();
    void toggleAutomaticallyPan();
    void toggleLoopPlayback();

    void openPlaybackSetupDialog();

    // void addLoopBoundary(LoopBoundaryType type);
    // void addLoopBoundaryToTick(LoopBoundaryType type, int tick);
    // void updateLoop();

    void enableLoop();
    void disableLoop();

    void notifyActionCheckedChanged(const muse::actions::ActionCode& actionCode);

    void subscribeOnAudioParamsChanges();
    void setupSequenceTracks();
    void setupSequencePlayer();

    void initMuteStates();

    void updateSoloMuteStates();
    void updateAuxMuteStates();

    void setCurrentPlaybackTime(audio::msecs_t msecs);

    using TrackAddFinished = std::function<void ()>;

    audio::msecs_t tickToMsecs(int tick) const;

    muse::async::Notification m_isPlayAllowedChanged;
    muse::async::Notification m_isPlayingChanged;
    muse::async::Notification m_playbackPositionChanged;
    muse::async::Notification m_totalPlayTimeChanged;
    muse::async::Notification m_currentTempoChanged;
    muse::async::Channel<uint32_t> m_tickPlayed;
    muse::async::Channel<muse::actions::ActionCode> m_actionCheckedChanged;

    muse::async::Notification m_currentSequenceIdChanged;
    audio::PlaybackStatus m_currentPlaybackStatus = audio::PlaybackStatus::Stopped;
    audio::msecs_t m_currentPlaybackTimeMsecs = 0;

    muse::async::Channel<audio::TrackId> m_trackAdded;
    muse::async::Channel<audio::TrackId> m_trackRemoved;

    muse::async::Channel<audio::aux_channel_idx_t, std::string> m_auxChannelNameChanged;

    muse::Progress m_loadingProgress;
    size_t m_loadingTrackCount = 0;

    bool m_isExportingAudio = false;
    bool m_isRangeSelection = false;
};
}

#endif // MU_PLAYBACK_PLAYBACKCONTROLLER_H
