/*
* Audacity: A Digital Audio Editor
*/
#include "playbackcontroller.h"
#include "playbackuiactions.h"
#include "../playbacktypes.h"

using namespace muse;
using namespace au::audio;
using namespace au::playback;
using namespace muse::async;
using namespace muse::actions;

static const ActionQuery PLAYBACK_TOGGLE_PLAY_PAUSE_QUERY("action://playback/toggle-play-pause");
static const ActionQuery PLAYBACK_TOGGLE_PLAY_STOP_QUERY("action://playback/toggle-play-stop");
static const ActionQuery PLAYBACK_TOGGLE_PLAY_FROM_CURSOR_QUERY("action://playback/toggle-play-from-cursor");
static const ActionQuery PLAYBACK_PLAY_SELECTION_QUERY("action://playback/play-selection");
static const ActionQuery PLAYBACK_PLAY_TRACKS_QUERY("action://playback/play-tracks");
static const ActionQuery PLAYBACK_PAUSE_QUERY("action://playback/pause");
static const ActionQuery PLAYBACK_STOP_QUERY("action://playback/stop");
static const ActionQuery PLAYBACK_REWIND_START_QUERY("action://playback/rewind-start");
static const ActionQuery PLAYBACK_REWIND_END_QUERY("action://playback/rewind-end");
static const ActionQuery PLAYBACK_SEEK_QUERY("action://playback/seek");
static const ActionQuery PLAYBACK_CHANGE_PLAY_REGION_QUERY("action://playback/play-region-change");
static const ActionQuery PLAYBACK_CHANGE_AUDIO_API_QUERY("action://playback/change-api");
static const ActionQuery PLAYBACK_CHANGE_PLAYBACK_DEVICE_QUERY("action://playback/change-playback-device");
static const ActionQuery PLAYBACK_CHANGE_RECORDING_DEVICE_QUERY("action://playback/change-recording-device");
static const ActionQuery PLAYBACK_CHANGE_INPUT_CHANNELS_QUERY("action://playback/change-input-channels");

static const ActionQuery RECORD_PAUSE_QUERY("action://record/pause");
static const ActionQuery RECORD_STOP_QUERY("action://record/stop");

static const ActionCode PAN_CODE("pan");
static const ActionCode REPEAT_CODE("repeat");

static const secs_t TIME_EPS = secs_t(1 / 1000.0);

namespace {
QString audioConfigurationFailureMessage(ApplyStatus status)
{
    switch (status) {
    case ApplyStatus::Busy:
        return muse::qtrc("playback", "Audio settings are already being changed.");
    case ApplyStatus::InvalidConfiguration:
        return muse::qtrc("playback", "The selected audio settings are invalid.");
    case ApplyStatus::InvalidRouting:
        return muse::qtrc("playback", "The selected audio routing is invalid.");
    case ApplyStatus::NoUsableAudioApi:
        return muse::qtrc("playback", "No usable audio API is available.");
    case ApplyStatus::NoAsioDevice:
        return muse::qtrc("playback", "No ASIO device is available.");
    case ApplyStatus::OwnerUnavailable:
        return muse::qtrc("playback", "The active audio stream could not be stopped.");
    case ApplyStatus::InternalError:
        return muse::qtrc("playback", "An internal error occurred while changing the audio settings.");
    case ApplyStatus::Applied:
    case ApplyStatus::NoChange:
        return {};
    }
    return {};
}

QString audioConfigurationMessage(const ApplyResult& result,
                                  QString message,
                                  const QString& restorationFailure)
{
    if (result.streamRestorationFailed) {
        if (!message.isEmpty()) {
            message += " ";
        }
        message += restorationFailure;
    }
    return message;
}
}

void PlaybackController::init()
{
    dispatcher()->reg(this, PLAYBACK_TOGGLE_PLAY_PAUSE_QUERY, this, &PlaybackController::togglePlayPauseAction);
    dispatcher()->reg(this, PLAYBACK_TOGGLE_PLAY_STOP_QUERY, this, &PlaybackController::togglePlayStopAction);
    dispatcher()->reg(this, PLAYBACK_TOGGLE_PLAY_FROM_CURSOR_QUERY, this, &PlaybackController::togglePlayFromCursorAction);
    dispatcher()->reg(this, PLAYBACK_PLAY_SELECTION_QUERY, this, &PlaybackController::playSelectionAction);
    dispatcher()->reg(this, PLAYBACK_PLAY_TRACKS_QUERY, this, &PlaybackController::playTracksAction);
    dispatcher()->reg(this, PLAYBACK_PAUSE_QUERY, this, &PlaybackController::pauseAction);
    dispatcher()->reg(this, PLAYBACK_STOP_QUERY, this, &PlaybackController::stopAction);
    dispatcher()->reg(this, PLAYBACK_REWIND_START_QUERY, this, &PlaybackController::rewindToStartAction);
    dispatcher()->reg(this, PLAYBACK_REWIND_END_QUERY, this, &PlaybackController::rewindToEndAction);
    dispatcher()->reg(this, PLAYBACK_SEEK_QUERY, this, &PlaybackController::onSeekAction);
    dispatcher()->reg(this, PLAYBACK_CHANGE_PLAY_REGION_QUERY, this, &PlaybackController::onChangePlaybackRegionAction);
    dispatcher()->reg(this, PLAYBACK_CHANGE_AUDIO_API_QUERY, this, &PlaybackController::setAudioApi);
    dispatcher()->reg(this, PLAYBACK_CHANGE_PLAYBACK_DEVICE_QUERY, this, &PlaybackController::setAudioOutputDevice);
    dispatcher()->reg(this, PLAYBACK_CHANGE_RECORDING_DEVICE_QUERY, this, &PlaybackController::setAudioInputDevice);
    dispatcher()->reg(this, PLAYBACK_CHANGE_INPUT_CHANNELS_QUERY, this, &PlaybackController::setInputChannels);

    dispatcher()->reg(this, REPEAT_CODE, this, &PlaybackController::togglePlayRepeats);
    dispatcher()->reg(this, PAN_CODE, this, &PlaybackController::toggleAutomaticallyPan);

    dispatcher()->reg(this, "toggle-loop-region", this, &PlaybackController::toggleLoopPlayback);
    dispatcher()->reg(this, "clear-loop-region", this, &PlaybackController::clearLoopRegion);
    dispatcher()->reg(this, "set-loop-region-to-selection", this, &PlaybackController::setLoopRegionToSelection);
    dispatcher()->reg(this, "set-selection-to-loop", this, &PlaybackController::setSelectionToLoop);
    dispatcher()->reg(this, "set-loop-region-in-out", this, &PlaybackController::setLoopRegionInOut);
    dispatcher()->reg(this, "toggle-selection-follows-loop-region", this, &PlaybackController::setSelectionFollowsLoopRegion);

    dispatcher()->reg(this, "rescan-devices", this, &PlaybackController::rescanAudioDevices);

    globalContext()->currentProjectChanged().onNotify(this, [this]() {
        onProjectChanged();
    });

    m_player = playback()->player();
    globalContext()->setPlayer(player());

    player()->playbackStatusChanged().onReceive(this, [this](PlaybackStatus) {
        m_isPlayingChanged.notify();
    });

    // No need to assert that we're on the main thread here: this is the init method of a controller...
    player()->playbackPositionChanged().onReceive(this, [this](const muse::secs_t&) {
        onPlaybackPositionChanged();
    });

    player()->loopRegionChanged().onNotify(this, [this](){
        m_actionCheckedChanged.send("toggle-loop-region");
        if (playbackConfiguration()->selectionFollowsLoopRegion()) {
            setSelectionToLoop();
        }
    });

    playbackConfiguration()->selectionFollowsLoopRegionChanged().onNotify(this, [this]() {
        m_actionCheckedChanged.send("toggle-selection-follows-loop-region");
    });

    recordController()->isRecordingChanged().onNotify(this, [this]() {
        m_isPlayAllowedChanged.notify();
    });

    audioDriverController()->usedOutputDeviceChanged().onReceive(this, [this](const std::string& device) {
        const std::string message = device.empty()
                                    ? muse::trc("playback", "No playback device is available.")
                                    : muse::qtrc("playback", "“%1” is now used for playback.")
                                    .arg(QString::fromStdString(device)).toStdString();
        toastService()->showInfo(muse::trc("playback", "Playback device changed"), message);
    });

    audioDriverController()->usedInputDeviceChanged().onReceive(this, [this](const std::string& device) {
        const std::string message = device.empty()
                                    ? muse::trc("playback", "No recording device is available.")
                                    : muse::qtrc("playback", "“%1” is now used for recording.")
                                    .arg(QString::fromStdString(device)).toStdString();
        toastService()->showInfo(muse::trc("playback", "Recording device changed"), message);
    });
}

void PlaybackController::deinit()
{
}

IPlayerPtr PlaybackController::player() const
{
    return m_player;
}

bool PlaybackController::isPlayAllowed() const
{
    return !recordController()->isRecording();
}

Notification PlaybackController::isPlayAllowedChanged() const
{
    return m_isPlayAllowedChanged;
}

bool PlaybackController::isPlaying() const
{
    //! NOTE: while recording (including the lead-in pre-roll) the audio is driven by the
    //! record stream, not the player. Report not-playing so every caller sees the same
    //! state as on the normal record path, where the player stays stopped throughout.
    //! Otherwise pausing/resuming the lead-in leaves the player "running" and, e.g., the
    //! record button gets disabled mid-recording.
    if (recordController()->isRecording()) {
        return false;
    }

    return player()->playbackStatus() == PlaybackStatus::Running;
}

bool PlaybackController::isPaused() const
{
    return player()->playbackStatus() == PlaybackStatus::Paused;
}

bool PlaybackController::isStopped() const
{
    return player()->playbackStatus() == PlaybackStatus::Stopped;
}

bool PlaybackController::isLoaded() const
{
    return m_loadingTrackCount == 0;
}

bool PlaybackController::isLoopRegionActive() const
{
    au::project::IAudacityProjectPtr prj = globalContext()->currentProject();

    return prj ? player()->isLoopRegionActive() : false;
}

PlaybackRegion PlaybackController::selectionPlaybackRegion() const
{
    if (!selectionController()->timeSelectionIsEmpty()) {
        return { selectionController()->dataSelectedStartTime(),
                 selectionController()->dataSelectedEndTime() };
    }

    return PlaybackRegion();
}

bool PlaybackController::isPlaybackRegionChanged() const
{
    if (isLoopRegionActive()) {
        //! NOTE: with an active loop region the player's play region is the loop region,
        //! not the last requested playback region — the comparison below would always
        //! report a change and resuming from pause would restart playback instead
        return false;
    }

    return m_lastPlaybackRegion.isValid() && m_lastPlaybackRegion != player()->playbackRegion();
}

void PlaybackController::updatePlaybackRegion()
{
    player()->setPlaybackRegion(m_lastPlaybackRegion);
}

Notification PlaybackController::isPlayingChanged() const
{
    return m_isPlayingChanged;
}

muse::secs_t PlaybackController::lastPlaybackSeekTime() const
{
    return m_lastPlaybackSeekTime;
}

muse::async::Notification PlaybackController::lastPlaybackSeekTimeChanged() const
{
    return m_lastPlaybackSeekTimeChanged;
}

PlaybackStatus PlaybackController::playbackStatus() const
{
    return player()->playbackStatus();
}

void PlaybackController::seek(const muse::secs_t secs, bool applyIfPlaying)
{
    IF_ASSERT_FAILED(player()) {
        return;
    }

    m_pausedResumePos.reset();
    player()->seek(secs, applyIfPlaying);
}

void PlaybackController::stopSeekAndUpdatePlaybackRegion()
{
    stop();

    seek(lastPlaybackSeekTime(), false);
    updatePlaybackRegion();
}

Channel<uint32_t> PlaybackController::midiTickPlayed() const
{
    return m_tickPlayed;
}

muse::async::Channel<au::playback::TrackId> PlaybackController::trackAdded() const
{
    return m_trackAdded;
}

muse::async::Channel<au::playback::TrackId> PlaybackController::trackRemoved() const
{
    return m_trackRemoved;
}

// ISoloMuteState::SoloMuteState PlaybackController::trackSoloMuteState(const TrackId& trackId) const
// {
// }

// void PlaybackController::setTrackSoloMuteState(const TrackId& trackId,
//                                                const ISoloMuteState::SoloMuteState& state) const
// {
// }

void PlaybackController::onProjectChanged()
{
    au::project::IAudacityProjectPtr prj = globalContext()->currentProject();
    if (prj) {
        prj->aboutCloseBegin().onNotify(this, [this]() {
            stopSeekAndUpdatePlaybackRegion();
        });

        seek(0.0, false); // TODO: get the previous position from the project data
        setLastPlaybackSeekTime(playbackPosition());
    }
}

void PlaybackController::onPlaybackPositionChanged()
{
    if (isPlaybackPositionOnTheEndOfProject() || isPlaybackPositionOnTheEndOfPlaybackRegion()) {
        //! NOTE: just stop, without seek
        player()->stop();
        if (player()->playbackRegion() != m_lastPlaybackRegion && !isEqualToPlaybackPosition(m_lastPlaybackRegion.end)) {
            // we want to update the playback region in case user made new selection during playback
            updatePlaybackRegion();
        }
    }
}

void PlaybackController::togglePlayPauseAction()
{
    //! NOTE: while recording, the play/pause button pauses the recorder so it stays a
    //! single action.
    if (!recordController()->isRecording()) {
        togglePlay(TogglePlayMode::PlayPause);
        return;
    }

    if (recordController()->isLeadInRecording()) {
        //! NOTE: during the lead-in pre-roll the audio is driven by the record stream, not by
        //! the player, so its status is not Running and togglePlay() can't see it as playing.
        //! Toggle the shared stream directly: pause it, or resume it if already paused.
        isPaused() ? doResume() : doPause();
    } else {
        dispatcher()->dispatch(RECORD_PAUSE_QUERY);
    }
}

void PlaybackController::togglePlayStopAction()
{
    togglePlay(TogglePlayMode::PlayStop);
}

void PlaybackController::togglePlayFromCursorAction()
{
    togglePlay(TogglePlayMode::PlayFromCursor);
}

void PlaybackController::togglePlay(TogglePlayMode mode)
{
    if (!isPlayAllowed()) {
        LOGW() << "playback not allowed";
        return;
    }

    const bool clearPlaybackRegion = mode == TogglePlayMode::PlayFromCursor;

    if (isPlaying()) {
        if (mode == TogglePlayMode::PlayStop) {
            stopSeekAndUpdatePlaybackRegion();
        } else {
            doPause();
        }

        return;
    }

    if (isPaused()) {
        if (isPlaybackRegionChanged()) {
            //! NOTE: just stop, without seek
            player()->stop();
            doPlay(false);
        } else if (clearPlaybackRegion) {
            //! NOTE: set the current position as start position
            doSeek(playbackPosition(), false);
            doPlay(true /* clearPlaybackRegion */);
        } else {
            doResume();
        }

        return;
    }

    if (isStopped()) {
        if (isPlaybackPositionOnTheEndOfProject()) {
            //! NOTE: reached the project end — restart from the beginning
            doSeek(0.0, false);
        } else if (isPlaybackPositionOnTheEndOfPlaybackRegion()) {
            //! NOTE: reached the end of a played selection/region — continue from the
            //! playhead rather than the region start, so the next play resumes where it
            //! left off instead of jumping back
            doSeek(playbackPosition(), false);
        }

        doPlay(clearPlaybackRegion);
    }
}

void PlaybackController::doPlay(bool clearPlaybackRegion)
{
    IF_ASSERT_FAILED(player()) {
        return;
    }

    if (m_pausedResumePos) {
        // Resuming a stream that a device change tore down while paused: start
        // at the pause position, leaving the play region and the seek anchor
        // untouched. Any explicit reposition since the teardown (seek, region
        // change, stop, play-selection) has already cleared the pending
        // position; a project that shrank below it makes it unplayable.
        const muse::secs_t position = *m_pausedResumePos;
        m_pausedResumePos.reset();
        if (position < totalPlayTime()) {
            player()->play(position);
            return;
        }
    }

    if (!clearPlaybackRegion) {
        //! NOTE: play from the cursor to the project end
        const muse::secs_t end = totalPlayTime();
        const muse::secs_t start = lastPlaybackSeekTime();
        if (end > start) {
            doChangePlaybackRegion({ start, end });
        } else {
            LOGW() << "playback region is not valid";
            updatePlaybackRegion();
        }
    } else {
        //! NOTE: no playback region; play from the cursor with no defined end
        doChangePlaybackRegion({});
        doSeek(lastPlaybackSeekTime(), false);
    }

    if (!isPlaybackStartPositionValid()) {
        return;
    }

    if (isStopped()) {
        //! NOTE: pass the start explicitly: when a loop region is active the play region
        //! cannot be updated, and playback must still start from the playhead, not from
        //! the loop region start
        player()->play(lastPlaybackSeekTime());
    } else {
        player()->play();
    }
}

void PlaybackController::playSelectionAction()
{
    if (!isPlayAllowed()) {
        LOGW() << "playback not allowed";
        return;
    }

    if (!isStopped()) {
        //! NOTE: just stop, without seek
        stop();
    }

    const PlaybackRegion selection = selectionPlaybackRegion();
    if (!selection.isValid()) {
        return;
    }

    doChangePlaybackRegion(selection);

    if (!isPlaybackStartPositionValid()) {
        return;
    }

    if (isLoopRegionActive()) {
        //! NOTE: the play region cannot be updated while a loop region is active —
        //! play the selected range directly so the selection is played, not the loop region
        player()->playRange(selection);
    } else {
        player()->play();
    }
}

void PlaybackController::playTracksAction(const muse::actions::ActionQuery&)
{
    // this is not implemented yet
    /*
    IF_ASSERT_FAILED(q.contains("trackList")) {
        return;
    }
    IF_ASSERT_FAILED(q.contains("startTime")) {
        return;
    }
    IF_ASSERT_FAILED(q.contains("endTime")) {
        return;
    }
    IF_ASSERT_FAILED(q.contains("options")) {
        return;
    }

    const std::shared_ptr<TrackList> trackList = q.param("trackList").toObject<TrackList>();
    const double startTime = q.param("startTime").toDouble();
    const double endTime = q.param("endTime").toDouble();
    const PlayTracksOptions options = q.param("options").toObject<PlayTracksOptions>();
    muse::Ret ret = player()->playTracks(*trackList, startTime, endTime, options);
    if (!ret.success()) {
        LOGE() << "playTracks failed: " << ret.toString();
    }
    */
}

void PlaybackController::rewindToStartAction()
{
    //! NOTE: In Audacity 3 we can't rewind while playing
    stopSeekAndUpdatePlaybackRegion();

    doSeek(0.0, false);

    selectionController()->resetTimeSelection();
}

void PlaybackController::rewindToEndAction()
{
    //! NOTE: In Audacity 3 we can't rewind while playing
    setLastPlaybackSeekTime(totalPlayTime());
    m_lastPlaybackRegion = { totalPlayTime(), totalPlayTime() };
    stopSeekAndUpdatePlaybackRegion();

    selectionController()->resetTimeSelection();
}

void PlaybackController::onSeekAction(const muse::actions::ActionQuery& q)
{
    IF_ASSERT_FAILED(q.contains("seekTime")) {
        return;
    }
    IF_ASSERT_FAILED(q.contains("triggerPlay")) {
        return;
    }

    if (recordController()->isRecording()) {
        return;
    }

    const muse::secs_t secs = q.param("seekTime").toDouble();
    const bool triggerPlay = q.param("triggerPlay").toBool();

    const bool isSeekStartPositionValid = isSeekPositionValid(secs);

    if (isPaused() || (!isSeekStartPositionValid)) {
        player()->stop();
    }

    doSeek(secs, triggerPlay);

    if (triggerPlay) {
        if (isPlaying()) {
            return;
        }

        if (!isSeekStartPositionValid) {
            return;
        }

        player()->play();
    }
}

void PlaybackController::doSeek(const muse::secs_t secs, bool applyIfPlaying)
{
    seek(secs, applyIfPlaying);
    setLastPlaybackSeekTime(secs);
    m_lastPlaybackRegion = { secs, secs };
    m_pauseShouldStopPlayback = false;
}

void PlaybackController::onChangePlaybackRegionAction(const muse::actions::ActionQuery& q)
{
    IF_ASSERT_FAILED(q.contains("start")) {
        return;
    }
    IF_ASSERT_FAILED(q.contains("end")) {
        return;
    }

    const muse::secs_t start = q.param("start").toDouble();
    const muse::secs_t end = q.param("end").toDouble();

    doChangePlaybackRegion({ start, end });
}

void PlaybackController::doChangePlaybackRegion(const PlaybackRegion& region)
{
    m_pausedResumePos.reset();
    m_lastPlaybackRegion = region;

    if (isStopped()) {
        updatePlaybackRegion();
    }

    if (region.isValid()) {
        setLastPlaybackSeekTime(m_lastPlaybackRegion.start);
    }
}

void PlaybackController::pauseAction()
{
    doPause();
}

void PlaybackController::doPause()
{
    IF_ASSERT_FAILED(player()) {
        return;
    }

    if (m_pauseShouldStopPlayback && isPlaying()) {
        m_pauseShouldStopPlayback = false;
        stopSeekAndUpdatePlaybackRegion();
        return;
    }

    player()->pause();
}

void PlaybackController::stopAction()
{
    //! NOTE: the stop button is a single action; the controller decides whether it
    //! stops the recorder or the player.
    if (recordController()->isRecording()) {
        dispatcher()->dispatch(RECORD_STOP_QUERY);
        return;
    }

    stopSeekAndUpdatePlaybackRegion();
}

void PlaybackController::stop()
{
    IF_ASSERT_FAILED(player()) {
        return;
    }
    m_pauseShouldStopPlayback = false;
    m_pausedResumePos.reset();
    player()->stop();
}

AudioStreamRestorer PlaybackController::suspendForAudioConfiguration(AudioStreamKind streamKind)
{
    const auto suspendRecording = [this]() -> AudioStreamRestorer {
        // Recording is intentionally not resumed after reconfiguration.
        if (!record()->stop() || !ensurePhysicalStreamStopped()) {
            return {};
        }
        return [] { return true; };
    };

    if (recordController()->isRecording()) {
        return suspendRecording();
    }

    const bool wasPlaying = isPlaying();
    const bool wasPaused = isPaused();
    if (wasPlaying || wasPaused) {
        const muse::secs_t position = player()->playbackPosition();
        stop();
        if (!ensurePhysicalStreamStopped()) {
            return {};
        }
        if (wasPaused) {
            m_pausedResumePos = position;
        }
        return [this, wasPlaying, position]() {
            if (!wasPlaying) {
                return true;
            }
            player()->play(position);
            return isPlaying();
        };
    }

    switch (streamKind) {
    case AudioStreamKind::Recording:
        return suspendRecording();

    case AudioStreamKind::Monitoring: {
        const auto project = globalContext()->currentProject();
        if (!project) {
            return {};
        }
        auto au3Project = reinterpret_cast<AudacityProject*>(project->au3ProjectPtr());
        audioEngine()->stopMonitoring();
        if (!ensurePhysicalStreamStopped()) {
            return {};
        }
        return [this, au3Project]() {
                if (audioDriverController()->inputDevices().empty() || audioDriverController()->inputChannelsAvailable() <= 0) {
                    return true;
                }

                audioEngine()->startMonitoring(*au3Project);
                return audioEngine()->isMonitoring();
            };
    }

    case AudioStreamKind::Playback:
        if (!ensurePhysicalStreamStopped()) {
            return {};
        }
        return [] { return true; };
    }
    return {};
}

bool PlaybackController::ensurePhysicalStreamStopped()
{
    if (!audioEngine()) {
        return false;
    }
    if (audioEngine()->currentStream()) {
        audioEngine()->stopStream();
    }
    return !audioEngine()->currentStream();
}

void PlaybackController::doResume()
{
    IF_ASSERT_FAILED(player()) {
        return;
    }

    player()->resume();
}

void PlaybackController::togglePlayRepeats()
{
    NOT_IMPLEMENTED;

    // configuration()->setIsPlayRepeatsEnabled(!playRepeatsEnabled);

    notifyActionCheckedChanged(REPEAT_CODE);
}

void PlaybackController::toggleAutomaticallyPan()
{
    NOT_IMPLEMENTED;

    // configuration()->setIsAutomaticallyPanEnabled(!panEnabled);

    notifyActionCheckedChanged(PAN_CODE);
}

void PlaybackController::toggleLoopPlayback()
{
    player()->setLoopRegionActive(!isLoopRegionActive());
    notifyActionCheckedChanged("toggle-loop-region");
}

PlaybackRegion PlaybackController::loopRegion() const
{
    return player()->loopRegion();
}

void PlaybackController::setLoopRegion(const PlaybackRegion& region)
{
    player()->setLoopRegion(region);
}

void PlaybackController::setLoopRegionStart(const muse::secs_t time)
{
    player()->setLoopRegionStart(time);
}

void PlaybackController::setLoopRegionEnd(const muse::secs_t time)
{
    player()->setLoopRegionEnd(time);
}

void PlaybackController::setLoopRegionActive(const bool active)
{
    player()->setLoopRegionActive(active);
}

void PlaybackController::clearLoopRegion()
{
    player()->clearLoopRegion();
}

void PlaybackController::setLastPlaybackSeekTime(muse::secs_t secs)
{
    if (muse::RealIsEqual(lastPlaybackSeekTime(), secs)) {
        return;
    }

    m_lastPlaybackSeekTime = secs;
    m_pauseShouldStopPlayback = isPlaying();
    m_lastPlaybackSeekTimeChanged.notify();
}

void PlaybackController::loopEditingBegin()
{
    player()->loopEditingBegin();
}

void PlaybackController::loopEditingEnd()
{
    player()->loopEditingEnd();
}

bool PlaybackController::isLoopRegionClear() const
{
    return player()->isLoopRegionClear();
}

muse::async::Notification PlaybackController::loopRegionChanged() const
{
    return player()->loopRegionChanged();
}

void PlaybackController::setLoopRegionToSelection()
{
    double start = 0;
    double end = 0;

    if (!selectionController()->timeSelectionIsEmpty()) {
        start = selectionController()->dataSelectedStartTime();
        end = selectionController()->dataSelectedEndTime();
    } else {
        auto itemStart = selectionController()->leftMostSelectedItemStartTime();
        auto itemEnd = selectionController()->rightMostSelectedItemEndTime();
        if (itemStart.has_value() && itemEnd.has_value()) {
            start = itemStart.value();
            end = itemEnd.value();
        } else {
            player()->clearLoopRegion();
            return;
        }
    }

    player()->setLoopRegion({ start, end });
}

void PlaybackController::setSelectionToLoop()
{
    PlaybackRegion loopRegion = player()->loopRegion();

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    trackedit::TrackIdList tracks = prj->trackIdList();

    selectionController()->setSelectedTracks(tracks, false);
    selectionController()->setDataSelectedStartTime(loopRegion.start, false);
    selectionController()->setDataSelectedEndTime(loopRegion.end, true);
}

void PlaybackController::setLoopRegionInOut()
{
    PlaybackRegion region = player()->loopRegion();

    muse::UriQuery loopRegionInOutUri("audacity://playback/loop_region_in_out");
    loopRegionInOutUri.addParam("title", muse::Val(muse::trc("trackedit", "Set looping region in/out")));
    loopRegionInOutUri.addParam("start", muse::Val(static_cast<double>(region.start)));
    loopRegionInOutUri.addParam("end", muse::Val(static_cast<double>(region.end)));

    RetVal<Val> rv = interactive()->openSync(loopRegionInOutUri);
    if (!rv.ret.success()) {
        return;
    }

    QVariantMap vals = rv.val.toQVariant().toMap();

    player()->setLoopRegion({ vals["start"].toDouble(), vals["end"].toDouble() });
}

void PlaybackController::setSelectionFollowsLoopRegion()
{
    playbackConfiguration()->setSelectionFollowsLoopRegion(!playbackConfiguration()->selectionFollowsLoopRegion());
}

void PlaybackController::setAudioApi(const muse::actions::ActionQuery& q)
{
    IF_ASSERT_FAILED(q.contains("api_index")) {
        return;
    }

    const int index = q.param("api_index").toInt();
    const auto values = audioDriverController()->apis();
    if (index < 0 || static_cast<size_t>(index) >= values.size()) {
        return;
    }
    AudioConfigurationChange change;
    change.api = values[index];
    handleAudioConfigurationResult(audioDriverController()->apply(iocContext(), change),
                                   PLAYBACK_CHANGE_AUDIO_API_QUERY.toString());
}

void PlaybackController::setAudioOutputDevice(const muse::actions::ActionQuery& q)
{
    AudioConfigurationChange change;
    if (q.param("is_default_device", muse::Val(false)).toBool()) {
        change.outputDevice = AudioDeviceSelection {};
    } else {
        IF_ASSERT_FAILED(q.contains("device_index")) {
            return;
        }

        const int index = q.param("device_index").toInt();
        const auto values = audioDriverController()->outputDevices();
        if (index < 0 || static_cast<size_t>(index) >= values.size()) {
            return;
        }
        change.outputDevice = values[index];
    }
    handleAudioConfigurationResult(audioDriverController()->apply(iocContext(), change),
                                   PLAYBACK_CHANGE_PLAYBACK_DEVICE_QUERY.toString());
}

void PlaybackController::setAudioInputDevice(const muse::actions::ActionQuery& q)
{
    AudioConfigurationChange change;
    if (q.param("is_default_device", muse::Val(false)).toBool()) {
        change.inputDevice = AudioDeviceSelection {};
    } else {
        IF_ASSERT_FAILED(q.contains("device_index")) {
            return;
        }

        const int index = q.param("device_index").toInt();
        const auto values = audioDriverController()->inputDevices();
        if (index < 0 || static_cast<size_t>(index) >= values.size()) {
            return;
        }
        change.inputDevice = values[index];
    }
    handleAudioConfigurationResult(audioDriverController()->apply(iocContext(), change),
                                   PLAYBACK_CHANGE_RECORDING_DEVICE_QUERY.toString());
}

void PlaybackController::setInputChannels(const muse::actions::ActionQuery& q)
{
    IF_ASSERT_FAILED(q.contains("input-channels_index")) {
        return;
    }

    const int channels = q.param("input-channels_index").toInt();
    AudioConfigurationChange change;
    change.inputChannels = channels;
    handleAudioConfigurationResult(audioDriverController()->apply(iocContext(), change),
                                   PLAYBACK_CHANGE_INPUT_CHANNELS_QUERY.toString());
}

void PlaybackController::rescanAudioDevices()
{
    const auto result = audioDriverController()->rescan();
    if (!result.succeeded() && interactive()) {
        const auto message = audioConfigurationMessage(
            result,
            audioConfigurationFailureMessage(result.status),
            muse::qtrc("playback", "The previous audio state could not be restored."));
        interactive()->error(muse::qtrc("playback", "Unable to rescan audio devices").toStdString(),
                             message.toStdString());
    } else {
        const auto notice = audioConfigurationMessage(
            result,
            {},
            muse::qtrc("playback", "The audio stream could not be restored after rescanning audio devices."));
        if (!notice.isEmpty() && interactive()) {
            interactive()->warning(muse::qtrc("playback", "Audio devices").toStdString(),
                                   notice.toStdString());
        }
    }
}

void PlaybackController::handleAudioConfigurationResult(const ApplyResult& result, const ActionCode& actionCode)
{
    if (!result.succeeded()) {
        // Restore the check state optimistically changed by the menu.
        notifyActionCheckedChanged(actionCode);
        if (interactive()) {
            const auto message = audioConfigurationMessage(
                result,
                audioConfigurationFailureMessage(result.status),
                muse::qtrc("playback", "The previous audio state could not be restored."));
            interactive()->error(muse::qtrc("playback", "Unable to change audio settings").toStdString(),
                                 message.toStdString());
        }
        return;
    }

    const auto notice = audioConfigurationMessage(
        result,
        {},
        muse::qtrc("playback", "The audio stream could not be restored after changing the audio settings."));
    if (!notice.isEmpty() && interactive()) {
        interactive()->warning(muse::qtrc("playback", "Audio settings").toStdString(),
                               notice.toStdString());
    }
}

void PlaybackController::notifyActionCheckedChanged(const ActionCode& actionCode)
{
    m_actionCheckedChanged.send(actionCode);
}

void PlaybackController::subscribeOnAudioParamsChanges()
{
    NOT_IMPLEMENTED;
}

void PlaybackController::initMuteStates()
{
    NOT_IMPLEMENTED;
}

void PlaybackController::updateSoloMuteStates()
{
    NOT_IMPLEMENTED;
}

bool PlaybackController::isEqualToPlaybackPosition(const secs_t position) const
{
    const secs_t playbackPos = playbackPosition();
    return playbackPos - TIME_EPS <= position && position <= playbackPos + TIME_EPS;
}

bool PlaybackController::isPlaybackPositionOnTheEndOfProject() const
{
    return isEqualToPlaybackPosition(totalPlayTime());
}

bool PlaybackController::isPlaybackPositionOnTheEndOfPlaybackRegion() const
{
    PlaybackRegion playbackRegion = player()->playbackRegion();
    return playbackRegion.isValid() && isEqualToPlaybackPosition(playbackRegion.end) && !isLoopRegionActive();
}

bool PlaybackController::isPlaybackStartPositionValid() const
{
    muse::secs_t totalPlayTime = this->totalPlayTime();

    if (lastPlaybackSeekTime() >= totalPlayTime) {
        return false;
    }

    if (m_lastPlaybackRegion.start >= totalPlayTime) {
        return false;
    }

    return true;
}

bool PlaybackController::isSeekPositionValid(const muse::secs_t& seekTime) const
{
    const auto playbackRegion = player()->playbackRegion();
    return playbackRegion.isValid() ? (seekTime <= playbackRegion.end) : (seekTime <= totalPlayTime());
}

muse::secs_t PlaybackController::playbackPosition() const
{
    return player()->playbackPosition();
}

bool PlaybackController::actionChecked(const ActionCode& actionCode) const
{
    QMap<std::string, bool> isChecked {
        { "toggle-loop-region", isLoopRegionActive() },
        { "toggle-selection-follows-loop-region", playbackConfiguration()->selectionFollowsLoopRegion() }
    };

    return isChecked[actionCode];
}

Channel<ActionCode> PlaybackController::actionCheckedChanged() const
{
    return m_actionCheckedChanged;
}

muse::secs_t PlaybackController::totalPlayTime() const
{
    project::IAudacityProjectPtr project = globalContext()->currentProject();
    if (!project) {
        return 0;
    }

    return project->trackeditProject()->totalTime();
}

Notification PlaybackController::totalPlayTimeChanged() const
{
    return m_totalPlayTimeChanged;
}

muse::Progress PlaybackController::loadingProgress() const
{
    return m_loadingProgress;
}

bool PlaybackController::canReceiveAction(const ActionCode& code) const
{
    // note that we currently do toString() on the NAMED_CODE because those are ActionQuery, and we don't have
    // convenient way to compare ActionCode with ActionQuery
    if (globalContext()->currentProject() == nullptr) {
        return false;
    }

    //! NOTE: toggle-play-pause stays available while recording — it pauses the recorder.
    //! Starting or restarting playback outright must not be possible while recording.
    if (code == PLAYBACK_TOGGLE_PLAY_STOP_QUERY.toString()
        || code == PLAYBACK_TOGGLE_PLAY_FROM_CURSOR_QUERY.toString()) {
        return !recordController()->isRecording();
    }

    if (code == PLAYBACK_PLAY_SELECTION_QUERY.toString()) {
        //! NOTE: when playback is active the action stops it, so it stays available without a selection
        return !recordController()->isRecording()
               && (!isStopped() || !selectionController()->timeSelectionIsEmpty());
    }

    if (code == PLAYBACK_REWIND_START_QUERY.toString() || code == PLAYBACK_REWIND_END_QUERY.toString()) {
        return !isPlaying() && !recordController()->isRecording();
    }

    return true;
}
