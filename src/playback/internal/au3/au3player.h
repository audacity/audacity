/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../../iplayer.h"

#include "global/async/asyncable.h"
#include "global/types/retval.h"
#include "global/timer.h"

#include "trackedit/iselectioncontroller.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "au3audio/iaudioengine.h"

#include "au3wrap/au3types.h"

struct TransportSequences;
namespace au::playback {
class Au3Player : public IPlayer, public muse::async::Asyncable
{
    muse::Inject<context::IGlobalContext> globalContext;
    muse::Inject<au::trackedit::ISelectionController> selectionController;
    muse::Inject<au::audio::IAudioEngine> audioEngine;

public:

    Au3Player();

    bool isBusy() const override;

    void play() override;
    void seek(const muse::secs_t newPosition, bool applyIfPlaying = false) override;
    void rewind() override;
    void stop() override;
    void pause() override;
    void resume() override;

    bool isRunning() const override;
    PlaybackStatus playbackStatus() const override;
    muse::async::Channel<PlaybackStatus> playbackStatusChanged() const override;
    muse::ValNt<bool> reachedEnd() const override;

    PlaybackRegion playbackRegion() const override;
    void setPlaybackRegion(const PlaybackRegion& region) override;

    PlaybackRegion loopRegion() const override;
    void loopEditingBegin() override;
    void loopEditingEnd() override;
    void setLoopRegion(const PlaybackRegion& region) override;
    void setLoopRegionStart(const muse::secs_t time) override;
    void setLoopRegionEnd(const muse::secs_t time) override;
    void clearLoopRegion() override;
    bool isLoopRegionClear() const override;
    muse::async::Notification loopRegionChanged() const override;

    bool isLoopRegionActive() const override;
    void setLoopRegionActive(const bool active) override;

    muse::secs_t playbackPosition() const override;
    muse::async::Channel<muse::secs_t> playbackPositionChanged() const override;

    muse::Ret playTracks(TrackList& trackList, double startTime, double endTime, const PlayTracksOptions& options = {}) override;

private:
    au3::Au3Project& projectRef() const;

    bool canStopAudioStream() const;

    TransportSequences makeTransportTracks(au3::Au3TrackList& trackList, bool selectedOnly);

    muse::Ret doPlayTracks(TrackList& trackList, double startTime, double endTime, const PlayTracksOptions& options = {});

    void updatePlaybackState();

    muse::async::Notification m_loopRegionChanged;

    muse::ValCh<PlaybackStatus> m_playbackStatus;
    muse::ValNt<bool> m_reachedEnd;

    muse::Timer m_positionUpdateTimer;
    muse::ValCh<muse::secs_t> m_playbackPosition;
    double m_startOffset = 0.0;
};
}
