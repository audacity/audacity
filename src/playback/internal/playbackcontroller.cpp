/*
* Audacity: A Digital Audio Editor
*/
#include "playbackcontroller.h"

#include "framework/rcommand/actiontocommand.h"

#include "record/recordcommands.h"

#include "playbackuiactions.h"
#include "../playbackcommands.h"
#include "../playbacktypes.h"

using namespace muse;
using namespace au::audio;
using namespace au::playback;
using namespace muse::async;
using namespace muse::actions;
using namespace muse::rcommand;

static const ActionQuery PLAYBACK_TOGGLE_PLAY_PAUSE_QUERY("action://playback/toggle-play-pause");
static const ActionQuery PLAYBACK_TOGGLE_PLAY_STOP_QUERY("action://playback/toggle-play-stop");
static const ActionQuery PLAYBACK_TOGGLE_PLAY_STOP_AND_SET_CURSOR_QUERY("action://playback/toggle-play-stop-and-set-cursor");
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

static const ActionCode PAN_CODE("pan");
static const ActionCode REPEAT_CODE("repeat");
static const ActionCode TOGGLE_LOOP_REGION_CODE("toggle-loop-region");
static const ActionCode CLEAR_LOOP_REGION_CODE("clear-loop-region");
static const ActionCode SET_LOOP_REGION_TO_SELECTION_CODE("set-loop-region-to-selection");
static const ActionCode SET_SELECTION_TO_LOOP_CODE("set-selection-to-loop");
static const ActionCode SET_LOOP_REGION_IN_OUT_CODE("set-loop-region-in-out");
static const ActionCode TOGGLE_SELECTION_FOLLOWS_LOOP_REGION_CODE("toggle-selection-follows-loop-region");
static const ActionCode RESCAN_DEVICES_CODE("rescan-devices");
static const ActionCode TRACK_MUTE_CODE("track-mute");
static const ActionCode TRACK_SOLO_CODE("track-solo");
static const ActionCode MUTE_ALL_TRACKS_CODE("mute-all-tracks");
static const ActionCode UNMUTE_ALL_TRACKS_CODE("unmute-all-tracks");
static const ActionCode MUTE_TRACKS_CODE("mute-tracks");
static const ActionCode UNMUTE_TRACKS_CODE("unmute-tracks");

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

CommandQuery queryParamsConv(const Command& command, const ActionData& args)
{
    CommandQuery query(command);
    if (args.empty()) {
        return query;
    }

    const ActionQuery legacy(args.arg<std::string>(0));
    query.setParams(legacy.params());
    return query;
}
}

void PlaybackController::init()
{
    auto cd = commandDispatcher();
    cd->onRequest(this, PLAYBACK_TOGGLE_PLAY_PAUSE_COMMAND, [this]() { return togglePlayPauseAction(); });
    cd->onRequest(this, PLAYBACK_TOGGLE_PLAY_STOP_COMMAND, [this]() { return togglePlayStopAction(); });
    cd->onRequest(this, PLAYBACK_TOGGLE_PLAY_STOP_AND_SET_CURSOR_COMMAND, [this]() { return togglePlayStopAndSetCursorAction(); });
    cd->onRequest(this, PLAYBACK_PLAY_SELECTION_COMMAND, [this]() { return playSelectionAction(); });
    cd->onRequest(this, PLAYBACK_PLAY_TRACKS_COMMAND, [this](const Params& params) { return playTracksAction(params); });
    cd->onRequest(this, PLAYBACK_PAUSE_COMMAND, [this]() { return pauseAction(); });
    cd->onRequest(this, PLAYBACK_STOP_COMMAND, [this]() { return stopAction(); });
    cd->onRequest(this, PLAYBACK_REWIND_START_COMMAND, [this]() { return rewindToStartAction(); });
    cd->onRequest(this, PLAYBACK_REWIND_END_COMMAND, [this]() { return rewindToEndAction(); });
    cd->onRequest(this, PLAYBACK_SEEK_COMMAND, [this](const Params& params) { return onSeekAction(params); });
    cd->onRequest(this, PLAYBACK_CHANGE_PLAY_REGION_COMMAND, [this](const Params& params) { return onChangePlaybackRegionAction(params); });
    cd->onRequest(this, PLAYBACK_CHANGE_AUDIO_API_COMMAND, [this](const Params& params) { return setAudioApi(params); });
    cd->onRequest(this, PLAYBACK_CHANGE_PLAYBACK_DEVICE_COMMAND, [this](const Params& params) { return setAudioOutputDevice(params); });
    cd->onRequest(this, PLAYBACK_CHANGE_RECORDING_DEVICE_COMMAND, [this](const Params& params) { return setAudioInputDevice(params); });
    cd->onRequest(this, PLAYBACK_CHANGE_INPUT_CHANNELS_COMMAND, [this](const Params& params) { return setInputChannels(params); });
    cd->onRequest(this, PLAYBACK_RESCAN_DEVICES_COMMAND, [this]() { return rescanAudioDevices(); });

    cd->onRequest(this, PLAYBACK_TOGGLE_PLAY_REPEATS_COMMAND, [this]() { return togglePlayRepeats(); });
    cd->onRequest(this, PLAYBACK_TOGGLE_AUTOMATIC_PAN_COMMAND, [this]() { return toggleAutomaticallyPan(); });

    cd->onRequest(this, PLAYBACK_TOGGLE_LOOP_REGION_COMMAND, [this]() {
        toggleLoopPlayback();
        return make_ok();
    });
    cd->onRequest(this, PLAYBACK_CLEAR_LOOP_REGION_COMMAND, [this]() {
        clearLoopRegion();
        return make_ok();
    });
    cd->onRequest(this, PLAYBACK_SET_LOOP_REGION_TO_SELECTION_COMMAND, [this]() { return setLoopRegionToSelection(); });
    cd->onRequest(this, PLAYBACK_SET_SELECTION_TO_LOOP_COMMAND, [this]() { return setSelectionToLoop(); });
    cd->onRequest(this, PLAYBACK_SET_LOOP_REGION_IN_OUT_COMMAND, [this]() { return setLoopRegionInOut(); });
    cd->onRequest(this, PLAYBACK_TOGGLE_SELECTION_FOLLOWS_LOOP_REGION_COMMAND, [this]() { return setSelectionFollowsLoopRegion(); });

    cd->onRequest(this, PLAYBACK_TOGGLE_MUTE_FOCUSED_TRACK_COMMAND, [this]() { return toggleMuteFocusedTrack(); });
    cd->onRequest(this, PLAYBACK_TOGGLE_SOLO_FOCUSED_TRACK_COMMAND, [this]() { return toggleSoloFocusedTrack(); });
    cd->onRequest(this, PLAYBACK_MUTE_ALL_TRACKS_COMMAND, [this]() { return muteAllTracks(); });
    cd->onRequest(this, PLAYBACK_UNMUTE_ALL_TRACKS_COMMAND, [this]() { return unmuteAllTracks(); });
    cd->onRequest(this, PLAYBACK_MUTE_SELECTED_TRACKS_COMMAND, [this]() { return muteSelectedTracks(); });
    cd->onRequest(this, PLAYBACK_UNMUTE_SELECTED_TRACKS_COMMAND, [this]() { return unmuteSelectedTracks(); });

    //! Note: This table won't be necessary after the actions to commands complete refactor.
    //! It will be removed on https://github.com/audacity/audacity/issues/12321
    static const std::vector<ActionToCommand> actionToCommand = {
        { PLAYBACK_TOGGLE_PLAY_PAUSE_QUERY.toString(), PLAYBACK_TOGGLE_PLAY_PAUSE_COMMAND, {} },
        { PLAYBACK_TOGGLE_PLAY_STOP_QUERY.toString(), PLAYBACK_TOGGLE_PLAY_STOP_COMMAND, {} },
        { PLAYBACK_TOGGLE_PLAY_STOP_AND_SET_CURSOR_QUERY.toString(), PLAYBACK_TOGGLE_PLAY_STOP_AND_SET_CURSOR_COMMAND, {} },
        { PLAYBACK_PLAY_SELECTION_QUERY.toString(), PLAYBACK_PLAY_SELECTION_COMMAND, {} },
        { PLAYBACK_PLAY_TRACKS_QUERY.toString(), PLAYBACK_PLAY_TRACKS_COMMAND, queryParamsConv },
        { PLAYBACK_PAUSE_QUERY.toString(), PLAYBACK_PAUSE_COMMAND, {} },
        { PLAYBACK_STOP_QUERY.toString(), PLAYBACK_STOP_COMMAND, {} },
        { PLAYBACK_REWIND_START_QUERY.toString(), PLAYBACK_REWIND_START_COMMAND, {} },
        { PLAYBACK_REWIND_END_QUERY.toString(), PLAYBACK_REWIND_END_COMMAND, {} },
        { PLAYBACK_SEEK_QUERY.toString(), PLAYBACK_SEEK_COMMAND, queryParamsConv },
        { PLAYBACK_CHANGE_PLAY_REGION_QUERY.toString(), PLAYBACK_CHANGE_PLAY_REGION_COMMAND, queryParamsConv },
        { PLAYBACK_CHANGE_AUDIO_API_QUERY.toString(), PLAYBACK_CHANGE_AUDIO_API_COMMAND, queryParamsConv },
        { PLAYBACK_CHANGE_PLAYBACK_DEVICE_QUERY.toString(), PLAYBACK_CHANGE_PLAYBACK_DEVICE_COMMAND, queryParamsConv },
        { PLAYBACK_CHANGE_RECORDING_DEVICE_QUERY.toString(), PLAYBACK_CHANGE_RECORDING_DEVICE_COMMAND, queryParamsConv },
        { PLAYBACK_CHANGE_INPUT_CHANNELS_QUERY.toString(), PLAYBACK_CHANGE_INPUT_CHANNELS_COMMAND, queryParamsConv },
        { REPEAT_CODE, PLAYBACK_TOGGLE_PLAY_REPEATS_COMMAND, {} },
        { PAN_CODE, PLAYBACK_TOGGLE_AUTOMATIC_PAN_COMMAND, {} },
        { TOGGLE_LOOP_REGION_CODE, PLAYBACK_TOGGLE_LOOP_REGION_COMMAND, {} },
        { CLEAR_LOOP_REGION_CODE, PLAYBACK_CLEAR_LOOP_REGION_COMMAND, {} },
        { SET_LOOP_REGION_TO_SELECTION_CODE, PLAYBACK_SET_LOOP_REGION_TO_SELECTION_COMMAND, {} },
        { SET_SELECTION_TO_LOOP_CODE, PLAYBACK_SET_SELECTION_TO_LOOP_COMMAND, {} },
        { SET_LOOP_REGION_IN_OUT_CODE, PLAYBACK_SET_LOOP_REGION_IN_OUT_COMMAND, {} },
        { TOGGLE_SELECTION_FOLLOWS_LOOP_REGION_CODE, PLAYBACK_TOGGLE_SELECTION_FOLLOWS_LOOP_REGION_COMMAND, {} },
        { RESCAN_DEVICES_CODE, PLAYBACK_RESCAN_DEVICES_COMMAND, {} },
        { TRACK_MUTE_CODE, PLAYBACK_TOGGLE_MUTE_FOCUSED_TRACK_COMMAND, {} },
        { TRACK_SOLO_CODE, PLAYBACK_TOGGLE_SOLO_FOCUSED_TRACK_COMMAND, {} },
        { MUTE_ALL_TRACKS_CODE, PLAYBACK_MUTE_ALL_TRACKS_COMMAND, {} },
        { UNMUTE_ALL_TRACKS_CODE, PLAYBACK_UNMUTE_ALL_TRACKS_COMMAND, {} },
        { MUTE_TRACKS_CODE, PLAYBACK_MUTE_SELECTED_TRACKS_COMMAND, {} },
        { UNMUTE_TRACKS_CODE, PLAYBACK_UNMUTE_SELECTED_TRACKS_COMMAND, {} },
    };
    registerActionToCommand(this, actionToCommand, commandDispatcher(), dispatcher());

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
        m_loopRegionChanged.notify();
        m_actionCheckedChanged.send(TOGGLE_LOOP_REGION_CODE);
        if (playbackConfiguration()->selectionFollowsLoopRegion()) {
            setSelectionToLoop();
        }
    });

    playbackConfiguration()->selectionFollowsLoopRegionChanged().onNotify(this, [this]() {
        m_actionCheckedChanged.send(TOGGLE_SELECTION_FOLLOWS_LOOP_REGION_CODE);
    });

    selectionController()->dataSelectedStartTimeChanged().onReceive(this, [this](trackedit::secs_t) {
        onSelectionChanged();
    });

    selectionController()->dataSelectedEndTimeChanged().onReceive(this, [this](trackedit::secs_t) {
        onSelectionChanged();
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

void PlaybackController::stopAndSeekToLastSeekTime()
{
    stop();

    doSeek(lastPlaybackSeekTime(), false);
}

void PlaybackController::stopAndSeekToPlaybackPosition()
{
    const muse::secs_t stopPosition = playbackPosition();

    stop();

    doSeek(stopPosition, false);
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
            stopAndSeekToLastSeekTime();
        });

        doSeek(0.0, false); // TODO: get the previous position from the project data
    }
}

void PlaybackController::onSelectionChanged()
{
    if (isStopped() || !m_isPlayingSelection) {
        return;
    }

    const PlaybackRegion selection = selectionPlaybackRegion();
    if (selection.isValid()) {
        doChangePlaybackRegion(selection);
    }
}

void PlaybackController::onPlaybackPositionChanged()
{
    if (isPlaybackPositionOnTheEndOfProject() || isPlaybackPositionAtOrAfterPlaybackRegionEnd()) {
        //! NOTE: just stop, without seek
        player()->stop();
    }
}

muse::Ret PlaybackController::togglePlayPauseAction()
{
    //! NOTE: while recording, the play/pause button pauses the recorder so it stays a
    //! single action.
    if (!recordController()->isRecording()) {
        return togglePlay(TogglePlayMode::PlayPause);
    }

    if (recordController()->isLeadInRecording()) {
        //! NOTE: during the lead-in pre-roll the audio is driven by the record stream, not by
        //! the player, so its status is not Running and togglePlay() can't see it as playing.
        //! Toggle the shared stream directly: pause it, or resume it if already paused.
        isPaused() ? doResume() : doPause();
    } else {
        commandDispatcher()->dispatch(record::RECORD_PAUSE_COMMAND);
    }

    return make_ok();
}

muse::Ret PlaybackController::togglePlayStopAction()
{
    return togglePlay(TogglePlayMode::PlayStop);
}

muse::Ret PlaybackController::togglePlayStopAndSetCursorAction()
{
    return togglePlay(TogglePlayMode::PlayStopAndSetCursor);
}

muse::Ret PlaybackController::togglePlay(TogglePlayMode mode)
{
    if (!isPlayAllowed()) {
        LOGW() << "playback not allowed";
        return make_ret(Ret::Code::Busy);
    }

    if (isPlaying()) {
        switch (mode) {
        case TogglePlayMode::PlayStopAndSetCursor:
            stopAndSeekToPlaybackPosition();
            break;
        case TogglePlayMode::PlayStop:
            stopAndSeekToLastSeekTime();
            break;
        case TogglePlayMode::PlayPause:
            doPause();
            break;
        }

        return make_ok();
    }

    if (isPaused()) {
        doResume();
        return make_ok();
    }

    if (isStopped()) {
        if (isPlaybackPositionOnTheEndOfProject()) {
            //! NOTE: reached the project end — restart from the beginning
            doSeek(0.0, false);
        } else if (isPlaybackPositionAtOrAfterPlaybackRegionEnd()) {
            //! NOTE: reached the end of a played selection/region — continue from the
            //! playhead rather than the region start, so the next play resumes where it
            //! left off instead of jumping back
            doSeek(playbackPosition(), false);
        }

        doPlay();
    }

    return make_ok();
}

void PlaybackController::doPlay()
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

    m_isPlayingSelection = false;

    //! NOTE: play from the cursor to the project end
    const muse::secs_t end = totalPlayTime();
    const muse::secs_t start = lastPlaybackSeekTime();
    if (end > start) {
        doChangePlaybackRegion({ start, end });
    } else {
        LOGW() << "playback region is not valid";
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

muse::Ret PlaybackController::playSelectionAction()
{
    if (!isPlayAllowed()) {
        LOGW() << "playback not allowed";
        return make_ret(Ret::Code::Busy);
    }

    if (!isStopped()) {
        //! NOTE: just stop, without seek
        stop();
    }

    m_isPlayingSelection = false;

    const PlaybackRegion selection = selectionPlaybackRegion();
    if (!selection.isValid()) {
        return make_ret(Ret::Code::NotSupported);
    }

    doChangePlaybackRegion(selection);

    if (!isPlaybackStartPositionValid()) {
        return make_ret(Ret::Code::NotSupported);
    }

    if (isLoopRegionActive()) {
        //! NOTE: the play region cannot be updated while a loop region is active —
        //! play the selected range directly so the selection is played, not the loop region
        player()->playRange(selection);
    } else {
        player()->play();
    }

    m_isPlayingSelection = true;
    return make_ok();
}

muse::Ret PlaybackController::playTracksAction(const Params&)
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
    return make_ret(Ret::Code::NotImplemented);
}

muse::Ret PlaybackController::rewindToStartAction()
{
    //! NOTE: In Audacity 3 we can't rewind while playing
    stopAndSeekToLastSeekTime();

    doSeek(0.0, false);

    selectionController()->resetTimeSelection();
    return make_ok();
}

muse::Ret PlaybackController::rewindToEndAction()
{
    //! NOTE: In Audacity 3 we can't rewind while playing
    setLastPlaybackSeekTime(totalPlayTime());
    stopAndSeekToLastSeekTime();

    selectionController()->resetTimeSelection();
    return make_ok();
}

muse::Ret PlaybackController::onSeekAction(const Params& params)
{
    IF_ASSERT_FAILED(params.contains("seekTime")) {
        return make_ret(Ret::Code::BadArgs);
    }
    IF_ASSERT_FAILED(params.contains("triggerPlay")) {
        return make_ret(Ret::Code::BadArgs);
    }

    if (recordController()->isRecording()) {
        return make_ret(Ret::Code::Busy);
    }

    const muse::secs_t secs = params.at("seekTime").toDouble();
    const bool triggerPlay = params.at("triggerPlay").toBool();

    const bool isSeekStartPositionValid = isSeekPositionValid(secs);

    if (isPaused() || (!isSeekStartPositionValid)) {
        player()->stop();
    }

    doSeek(secs, triggerPlay);

    if (triggerPlay && !isPlaying() && isSeekStartPositionValid) {
        player()->play();
    }

    return make_ok();
}

void PlaybackController::doSeek(const muse::secs_t secs, bool applyIfPlaying)
{
    IF_ASSERT_FAILED(player()) {
        return;
    }

    m_pausedResumePos.reset();
    player()->seek(secs, applyIfPlaying);
    setLastPlaybackSeekTime(secs);
    m_pauseShouldStopPlayback = false;
    m_isPlayingSelection = false;
}

muse::Ret PlaybackController::onChangePlaybackRegionAction(const Params& params)
{
    IF_ASSERT_FAILED(params.contains("start")) {
        return make_ret(Ret::Code::BadArgs);
    }
    IF_ASSERT_FAILED(params.contains("end")) {
        return make_ret(Ret::Code::BadArgs);
    }

    const muse::secs_t start = params.at("start").toDouble();
    const muse::secs_t end = params.at("end").toDouble();

    doChangePlaybackRegion({ start, end });
    return make_ok();
}

void PlaybackController::doChangePlaybackRegion(const PlaybackRegion& region)
{
    m_pausedResumePos.reset();

    if (isStopped() || m_isPlayingSelection) {
        player()->setPlaybackRegion(region);
    }

    if (region.isValid()) {
        setLastPlaybackSeekTime(region.start);
    }
}

muse::Ret PlaybackController::pauseAction()
{
    doPause();
    return make_ok();
}

void PlaybackController::doPause()
{
    IF_ASSERT_FAILED(player()) {
        return;
    }

    if (m_pauseShouldStopPlayback && isPlaying()) {
        m_pauseShouldStopPlayback = false;
        stopAndSeekToLastSeekTime();
        return;
    }

    player()->pause();
}

muse::Ret PlaybackController::stopAction()
{
    //! NOTE: the stop button is a single action; the controller decides whether it
    //! stops the recorder or the player.
    if (recordController()->isRecording()) {
        commandDispatcher()->dispatch(record::RECORD_STOP_COMMAND);
        return make_ok();
    }

    stopAndSeekToLastSeekTime();
    return make_ok();
}

void PlaybackController::stop()
{
    IF_ASSERT_FAILED(player()) {
        return;
    }
    m_pauseShouldStopPlayback = false;
    m_pausedResumePos.reset();
    m_isPlayingSelection = false;
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

muse::Ret PlaybackController::togglePlayRepeats()
{
    NOT_IMPLEMENTED;

    // configuration()->setIsPlayRepeatsEnabled(!playRepeatsEnabled);

    notifyActionCheckedChanged(REPEAT_CODE);
    return make_ret(Ret::Code::NotImplemented);
}

muse::Ret PlaybackController::toggleAutomaticallyPan()
{
    NOT_IMPLEMENTED;

    // configuration()->setIsAutomaticallyPanEnabled(!panEnabled);

    notifyActionCheckedChanged(PAN_CODE);
    return make_ret(Ret::Code::NotImplemented);
}

muse::Ret PlaybackController::toggleMuteFocusedTrack()
{
    const trackedit::TrackId trackId = trackNavigationController()->focusedTrack();
    trackPlaybackControl()->setMuted(trackId, !trackPlaybackControl()->muted(trackId));
    return make_ok();
}

muse::Ret PlaybackController::toggleSoloFocusedTrack()
{
    const trackedit::TrackId trackId = trackNavigationController()->focusedTrack();
    trackPlaybackControl()->setSolo(trackId, !trackPlaybackControl()->solo(trackId));
    return make_ok();
}

muse::Ret PlaybackController::muteAllTracks()
{
    trackedit::ITrackeditProjectPtr project = globalContext()->currentTrackeditProject();
    if (!project) {
        return make_ret(Ret::Code::NotSupported);
    }

    trackPlaybackControl()->setMuted(project->trackIdList(), true);
    return make_ok();
}

muse::Ret PlaybackController::unmuteAllTracks()
{
    trackedit::ITrackeditProjectPtr project = globalContext()->currentTrackeditProject();
    if (!project) {
        return make_ret(Ret::Code::NotSupported);
    }

    trackPlaybackControl()->setMuted(project->trackIdList(), false);
    return make_ok();
}

muse::Ret PlaybackController::muteSelectedTracks()
{
    trackPlaybackControl()->setMuted(selectionController()->selectedTracks(), true);
    return make_ok();
}

muse::Ret PlaybackController::unmuteSelectedTracks()
{
    trackPlaybackControl()->setMuted(selectionController()->selectedTracks(), false);
    return make_ok();
}

void PlaybackController::toggleLoopPlayback()
{
    player()->setLoopRegionActive(!isLoopRegionActive());
    notifyActionCheckedChanged(TOGGLE_LOOP_REGION_CODE);
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
    return m_loopRegionChanged;
}

muse::Ret PlaybackController::setLoopRegionToSelection()
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
            return make_ok();
        }
    }

    player()->setLoopRegion({ start, end });
    return make_ok();
}

muse::Ret PlaybackController::setSelectionToLoop()
{
    PlaybackRegion loopRegion = player()->loopRegion();

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    trackedit::TrackIdList tracks = prj->trackIdList();

    selectionController()->setSelectedTracks(tracks, false);
    selectionController()->setDataSelectedStartTime(loopRegion.start, false);
    selectionController()->setDataSelectedEndTime(loopRegion.end, true);
    return make_ok();
}

muse::Ret PlaybackController::setLoopRegionInOut()
{
    PlaybackRegion region = player()->loopRegion();

    muse::UriQuery loopRegionInOutUri("audacity://playback/loop_region_in_out");
    loopRegionInOutUri.addParam("title", muse::Val(muse::trc("trackedit", "Set looping region in/out")));
    loopRegionInOutUri.addParam("start", muse::Val(static_cast<double>(region.start)));
    loopRegionInOutUri.addParam("end", muse::Val(static_cast<double>(region.end)));

    RetVal<Val> rv = interactive()->openSync(loopRegionInOutUri);
    if (!rv.ret.success()) {
        return make_ret(Ret::Code::Cancel);
    }

    QVariantMap vals = rv.val.toQVariant().toMap();

    player()->setLoopRegion({ vals["start"].toDouble(), vals["end"].toDouble() });
    return make_ok();
}

muse::Ret PlaybackController::setSelectionFollowsLoopRegion()
{
    playbackConfiguration()->setSelectionFollowsLoopRegion(!playbackConfiguration()->selectionFollowsLoopRegion());
    return make_ok();
}

muse::Ret PlaybackController::setAudioApi(const Params& params)
{
    IF_ASSERT_FAILED(params.contains("api_index")) {
        return make_ret(Ret::Code::BadArgs);
    }

    const int index = params.at("api_index").toInt();
    const auto values = audioDriverController()->apis();
    if (index < 0 || static_cast<size_t>(index) >= values.size()) {
        return make_ret(Ret::Code::BadArgs);
    }
    AudioConfigurationChange change;
    change.api = values[index];
    return handleAudioConfigurationResult(audioDriverController()->apply(iocContext(), change),
                                          PLAYBACK_CHANGE_AUDIO_API_QUERY.toString());
}

muse::Ret PlaybackController::setAudioOutputDevice(const Params& params)
{
    AudioConfigurationChange change;
    if (params.at("is_default_device", muse::Val(false)).toBool()) {
        change.outputDevice = AudioDeviceSelection {};
    } else {
        IF_ASSERT_FAILED(params.contains("device_index")) {
            return make_ret(Ret::Code::BadArgs);
        }

        const int index = params.at("device_index").toInt();
        const auto values = audioDriverController()->outputDevices();
        if (index < 0 || static_cast<size_t>(index) >= values.size()) {
            return make_ret(Ret::Code::BadArgs);
        }
        change.outputDevice = values[index];
    }
    return handleAudioConfigurationResult(audioDriverController()->apply(iocContext(), change),
                                          PLAYBACK_CHANGE_PLAYBACK_DEVICE_QUERY.toString());
}

muse::Ret PlaybackController::setAudioInputDevice(const Params& params)
{
    AudioConfigurationChange change;
    if (params.at("is_default_device", muse::Val(false)).toBool()) {
        change.inputDevice = AudioDeviceSelection {};
    } else {
        IF_ASSERT_FAILED(params.contains("device_index")) {
            return make_ret(Ret::Code::BadArgs);
        }

        const int index = params.at("device_index").toInt();
        const auto values = audioDriverController()->inputDevices();
        if (index < 0 || static_cast<size_t>(index) >= values.size()) {
            return make_ret(Ret::Code::BadArgs);
        }
        change.inputDevice = values[index];
    }
    return handleAudioConfigurationResult(audioDriverController()->apply(iocContext(), change),
                                          PLAYBACK_CHANGE_RECORDING_DEVICE_QUERY.toString());
}

muse::Ret PlaybackController::setInputChannels(const Params& params)
{
    IF_ASSERT_FAILED(params.contains("input-channels_index")) {
        return make_ret(Ret::Code::BadArgs);
    }

    const int channels = params.at("input-channels_index").toInt();
    AudioConfigurationChange change;
    change.inputChannels = channels;
    return handleAudioConfigurationResult(audioDriverController()->apply(iocContext(), change),
                                          PLAYBACK_CHANGE_INPUT_CHANNELS_QUERY.toString());
}

muse::Ret PlaybackController::rescanAudioDevices()
{
    const auto result = audioDriverController()->rescan();
    if (!result.succeeded()) {
        if (interactive()) {
            const auto message = audioConfigurationMessage(
                result,
                audioConfigurationFailureMessage(result.status),
                muse::qtrc("playback", "The previous audio state could not be restored."));
            interactive()->error(muse::qtrc("playback", "Unable to rescan audio devices").toStdString(),
                                 message.toStdString());
        }
        return make_ret(Ret::Code::UnknownError);
    }

    const auto notice = audioConfigurationMessage(
        result,
        {},
        muse::qtrc("playback", "The audio stream could not be restored after rescanning audio devices."));
    if (!notice.isEmpty() && interactive()) {
        interactive()->warning(muse::qtrc("playback", "Audio devices").toStdString(),
                               notice.toStdString());
    }
    return make_ok();
}

muse::Ret PlaybackController::handleAudioConfigurationResult(const ApplyResult& result, const ActionCode& actionCode)
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
        return make_ret(Ret::Code::UnknownError);
    }

    const auto notice = audioConfigurationMessage(
        result,
        {},
        muse::qtrc("playback", "The audio stream could not be restored after changing the audio settings."));
    if (!notice.isEmpty() && interactive()) {
        interactive()->warning(muse::qtrc("playback", "Audio settings").toStdString(),
                               notice.toStdString());
    }
    return make_ok();
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

bool PlaybackController::isPlaybackPositionAtOrAfterPlaybackRegionEnd() const
{
    const PlaybackRegion playbackRegion = player()->playbackRegion();
    return playbackRegion.isValid()
           && (isEqualToPlaybackPosition(playbackRegion.end) || playbackPosition() > playbackRegion.end)
           && !isLoopRegionActive();
}

bool PlaybackController::isPlaybackStartPositionValid() const
{
    return lastPlaybackSeekTime() < totalPlayTime();
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
        { TOGGLE_LOOP_REGION_CODE, isLoopRegionActive() },
        { TOGGLE_SELECTION_FOLLOWS_LOOP_REGION_CODE, playbackConfiguration()->selectionFollowsLoopRegion() }
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
        || code == PLAYBACK_TOGGLE_PLAY_STOP_AND_SET_CURSOR_QUERY.toString()) {
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
