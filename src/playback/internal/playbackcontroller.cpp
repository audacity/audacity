/*
* Audacity: A Digital Audio Editor
*/
#include "playbackcontroller.h"
#include "playbackuiactions.h"
#include "../playbacktypes.h"

#include <algorithm>

using namespace muse;
using namespace au::audio;
using namespace au::playback;
using namespace muse::async;
using namespace muse::actions;

static const ActionQuery PLAYBACK_PLAY_QUERY("action://playback/play");
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

static const secs_t TIME_EPS = secs_t(1 / 1000.0);

void PlaybackController::init()
{
    dispatcher()->reg(this, PLAYBACK_PLAY_QUERY, this, &PlaybackController::togglePlayAction);
    dispatcher()->reg(this, PLAYBACK_PLAY_TRACKS_QUERY, this, &PlaybackController::playTracksAction);
    dispatcher()->reg(this, PLAYBACK_PAUSE_QUERY, this, &PlaybackController::pauseAction);
    dispatcher()->reg(this, PLAYBACK_STOP_QUERY, this, &PlaybackController::stopAction);
    dispatcher()->reg(this, PLAYBACK_REWIND_START_QUERY, this, &PlaybackController::rewindToStartAction);
    dispatcher()->reg(this, PLAYBACK_REWIND_END_QUERY, this, &PlaybackController::rewindToEndAction);
    dispatcher()->reg(this, PLAYBACK_SEEK_QUERY, this, &PlaybackController::onSeekAction);
    dispatcher()->reg(this, PLAYBACK_CHANGE_PLAY_REGION_QUERY, this, &PlaybackController::onChangePlaybackRegionAction);
    dispatcher()->reg(this, PLAYBACK_CHANGE_AUDIO_API_QUERY, this, &PlaybackController::setAudioApiAction);
    dispatcher()->reg(this, PLAYBACK_CHANGE_PLAYBACK_DEVICE_QUERY, this, &PlaybackController::setAudioOutputDeviceAction);
    dispatcher()->reg(this, PLAYBACK_CHANGE_RECORDING_DEVICE_QUERY, this, &PlaybackController::setAudioInputDeviceAction);
    dispatcher()->reg(this, PLAYBACK_CHANGE_INPUT_CHANNELS_QUERY, this, &PlaybackController::setInputChannelsAction);

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

bool PlaybackController::isSelectionPlaybackRegionChanged() const
{
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

    // Any explicit reposition supersedes a pending paused-stream resume.
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

muse::async::Channel<TrackId> PlaybackController::trackAdded() const
{
    return m_trackAdded;
}

muse::async::Channel<TrackId> PlaybackController::trackRemoved() const
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

void PlaybackController::togglePlayAction()
{
    if (!isPlayAllowed()) {
        LOGW() << "playback not allowed";
        return;
    }

    const bool isShiftPressed = application()->keyboardModifiers().testFlag(Qt::ShiftModifier);
    if (isPlaying()) {
        if (isShiftPressed) {
            stopSeekAndUpdatePlaybackRegion();
        } else {
            doPause();
        }
    } else if (isPaused()) {
        if (isSelectionPlaybackRegionChanged()) {
            //! NOTE: just stop, without seek
            player()->stop();
            doPlay(false);
        } else if (isShiftPressed) {
            //! NOTE: set the current position as start position
            doSeek(playbackPosition(), false);
            doPlay(true /* ignoreSelection */);
        } else {
            doResume();
        }
    } else {
        if (isPlaybackPositionOnTheEndOfProject() || isPlaybackPositionOnTheEndOfPlaybackRegion()) {
            doSeek(0.0, false);
        }

        doPlay(isShiftPressed /* ignoreSelection */);
    }
}

void PlaybackController::doPlay(bool ignoreSelection)
{
    IF_ASSERT_FAILED(player()) {
        return;
    }

    if (m_pausedResumePos.has_value()) {
        const muse::secs_t pos = *m_pausedResumePos;
        m_pausedResumePos.reset();

        // A selection made since the device change (through any path, including
        // ones that dispatch no seek/region action) supersedes the stale resume
        // position, and a project that shrank below it makes it unplayable.
        const PlaybackRegion selectionRegion = ignoreSelection ? PlaybackRegion() : selectionPlaybackRegion();
        const bool selectionSupersedesResume = selectionRegion.isValid() && selectionRegion != m_lastPlaybackRegion;
        if (!selectionSupersedesResume && pos < totalPlayTime()) {
            // Resuming a stream that a device change tore down while paused: start
            // the stream at the pause position. The play region, seek anchor and the
            // rest of the session state are deliberately untouched.
            player()->play(pos);
            return;
        }
    }

    if (!ignoreSelection) {
        PlaybackRegion selectionRegion = selectionPlaybackRegion();
        if (selectionRegion.isValid()) {
            doChangePlaybackRegion(selectionRegion);
        } else {
            //! NOTE: no selection — fall back to the user's cursor (lastPlaybackSeekTime)
            //! and the project end.
            const muse::secs_t end = totalPlayTime();
            const muse::secs_t start = lastPlaybackSeekTime();
            if (end > start) {
                doChangePlaybackRegion({ start, end });
            } else {
                LOGW() << "playback region is not valid";
                updatePlaybackRegion();
            }
        }
    } else {
        doChangePlaybackRegion({});
        doSeek(lastPlaybackSeekTime(), false);
    }

    if (!isPlaybackStartPositionValid()) {
        return;
    }

    player()->play();
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
    // A new region is just as much an explicit reposition as a seek: it
    // supersedes any pending paused-stream resume.
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

void PlaybackController::setAudioApiAction(const muse::actions::ActionQuery& q)
{
    changeAudioDeviceFromQuery(q, "api_index", audioDevicesProvider()->apis(), [this](const std::string& api) {
        setAudioApi(api);
    });
}

void PlaybackController::setAudioOutputDeviceAction(const muse::actions::ActionQuery& q)
{
    changeAudioDeviceFromQuery(q, "device_index", audioDevicesProvider()->outputDevices(), [this](const std::string& device) {
        setAudioOutputDevice(device);
    });
}

void PlaybackController::setAudioInputDeviceAction(const muse::actions::ActionQuery& q)
{
    changeAudioDeviceFromQuery(q, "device_index", audioDevicesProvider()->inputDevices(), [this](const std::string& device) {
        setAudioInputDevice(device);
    });
}

void PlaybackController::changeAudioDeviceFromQuery(const muse::actions::ActionQuery& q, const std::string& indexParam,
                                                    const std::vector<std::string>& options,
                                                    const std::function<void(const std::string&)>& applyValue)
{
    IF_ASSERT_FAILED(q.contains(indexParam)) {
        return;
    }

    // Resolve and bounds-check the selection against the current list, then
    // forward the resolved value (not the index) so a list that changes
    // meanwhile can't make us pick the wrong entry or read out of range.
    const int index = q.param(indexParam).toInt();
    IF_ASSERT_FAILED(index >= 0 && index < static_cast<int>(options.size())) {
        return;
    }

    applyValue(options.at(index));
}

void PlaybackController::setInputChannelsAction(const muse::actions::ActionQuery& q)
{
    IF_ASSERT_FAILED(q.contains("input-channels_index")) {
        return;
    }

    //! NOTE: despite the param name, the senders pass the 1-based channel count.
    setInputChannels(q.param("input-channels_index").toInt());
}

void PlaybackController::setAudioApi(const std::string& api)
{
    if (api != audioDevicesProvider()->currentApi()) {
        withStreamRestart([this, api]() {
            audioDevicesProvider()->setApi(api);
        });
    }
}

void PlaybackController::setAudioOutputDevice(const std::string& device)
{
    if (device != audioDevicesProvider()->currentOutputDevice()) {
        withStreamRestart([this, device]() {
            audioDevicesProvider()->setOutputDevice(device);
        });
    }
}

void PlaybackController::setAudioInputDevice(const std::string& device)
{
    if (device != audioDevicesProvider()->currentInputDevice()) {
        withStreamRestart([this, device]() {
            audioDevicesProvider()->setInputDevice(device);
        });
    }
}

void PlaybackController::setInputChannels(int channels)
{
    const int available = audioDevicesProvider()->inputChannelsAvailable();
    if (available <= 0) {
        return;
    }

    // The request may come from a menu or list built before a device change
    // shrank the channel count, so clamp instead of asserting on stale input.
    channels = std::clamp(channels, 1, available);

    if (channels != audioDevicesProvider()->inputChannelsSelected()) {
        withStreamRestart([this, channels]() {
            audioDevicesProvider()->setInputChannels(channels);
        });
    }
}

void PlaybackController::setDefaultSampleRate(uint64_t rate)
{
    if (rate != audioDevicesProvider()->defaultSampleRate()) {
        withStreamRestart([this, rate]() {
            audioDevicesProvider()->setDefaultSampleRate(rate);
        });
    }
}

void PlaybackController::setBufferLength(double duration)
{
    if (!muse::RealIsEqual(duration, audioDevicesProvider()->bufferLength())) {
        withStreamRestart([this, duration]() {
            audioDevicesProvider()->setBufferLength(duration);
        });
    }
}

void PlaybackController::setLatencyCompensation(double value)
{
    // The compensation is baked into the capture stream at start and consumed
    // within the take's first moments, so a change can never apply to the take
    // in progress. Rather than letting the user believe otherwise, stop the
    // recording — and, recording being sensitive, don't auto-resume.
    // We may decide in the future to automatically restart the recording, but then
    // it'd probably have to be in a new clip.
    // Playback does not consume this value, so it is left running.
    if (!muse::RealIsEqual(value, audioDevicesProvider()->latencyCompensation())) {
        if (recordController()->isRecording()) {
            record()->stop();
        }
        audioDevicesProvider()->setLatencyCompensation(value);
    }
}

void PlaybackController::rescanAudioDevices()
{
    withStreamRestart([this]() {
        audioDevicesProvider()->rescan();
    });
}

void PlaybackController::withStreamRestart(const std::function<void()>& change)
{
    const bool isRecording = recordController()->isRecording();
    const bool wasPlaying = !isRecording && isPlaying();
    const bool wasPaused = !isRecording && isPaused();
    const muse::secs_t resumePos = player()->playbackPosition();

    if (isRecording) {
        record()->stop();
    } else if (wasPlaying || wasPaused) {
        stop();
    }

    change();

    // Only resume when the user was actively playing. A paused or recording
    // transport is torn down for the switch and then left stopped.
    if (wasPlaying) {
        // Restart the stream at the interrupted position. The play region, the
        // seek anchor and the rest of the session state are deliberately
        // untouched: a bounded region still ends where it did, an active loop
        // still wraps within its bounds, and a stop still returns to where
        // playback originally started.
        player()->play(resumePos);
    } else if (wasPaused) {
        // The paused stream can't survive the switch, so we end up stopped.
        // Remember the pause position so the next play resumes from there; the
        // stop anchor (m_lastPlaybackSeekTime) is deliberately left untouched, so
        // a stop still returns to where playback originally started.
        m_pausedResumePos = resumePos;
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

    if (code == PLAYBACK_PLAY_QUERY.toString()) {
        return !recordController()->isRecording();
    }

    if (code == PLAYBACK_REWIND_START_QUERY.toString() || code == PLAYBACK_REWIND_END_QUERY.toString()) {
        return !isPlaying() && !recordController()->isRecording();
    }

    return true;
}
