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

static const ActionQuery PLAYBACK_PLAY_CODE("action://playback/play");
static const ActionQuery PLAYBACK_PLAY_TRACKS_CODE("action://playback/play-tracks");
static const ActionQuery PLAYBACK_PAUSE_CODE("action://playback/pause");
static const ActionQuery PLAYBACK_STOP_CODE("action://playback/stop");
static const ActionQuery PLAYBACK_REWIND_START_CODE("action://playback/rewind-start");
static const ActionQuery PLAYBACK_REWIND_END_CODE("action://playback/rewind-end");
static const ActionQuery PLAYBACK_SEEK_CODE("action://playback/seek");
static const ActionQuery PLAYBACK_CHANGE_PLAY_REGION_CODE("action://playback/play-region-change");

static const ActionCode PAN_CODE("pan");
static const ActionCode REPEAT_CODE("repeat");

static const ActionQuery PLAYBACK_CHANGE_AUDIO_API_QUERY("action://playback/change-api");
static const ActionQuery PLAYBACK_CHANGE_PLAYBACK_DEVICE_QUERY("action://playback/change-playback-device");
static const ActionQuery PLAYBACK_CHANGE_RECORDING_DEVICE_QUERY("action://playback/change-recording-device");
static const ActionQuery PLAYBACK_CHANGE_INPUT_CHANNELS_QUERY("action://playback/change-input-channels");

static const secs_t TIME_EPS = secs_t(1 / 1000.0);

void PlaybackController::init()
{
    dispatcher()->reg(this, PLAYBACK_PLAY_CODE, this, &PlaybackController::togglePlayAction);
    dispatcher()->reg(this, PLAYBACK_PLAY_TRACKS_CODE, this, &PlaybackController::playTracksAction);
    dispatcher()->reg(this, PLAYBACK_PAUSE_CODE, this, &PlaybackController::pauseAction);
    dispatcher()->reg(this, PLAYBACK_STOP_CODE, this, &PlaybackController::stopAction);
    dispatcher()->reg(this, PLAYBACK_REWIND_START_CODE, this, &PlaybackController::rewindToStartAction);
    dispatcher()->reg(this, PLAYBACK_REWIND_END_CODE, this, &PlaybackController::rewindToEndAction);
    dispatcher()->reg(this, PLAYBACK_SEEK_CODE, this, &PlaybackController::onSeekAction);
    dispatcher()->reg(this, PLAYBACK_CHANGE_PLAY_REGION_CODE, this, &PlaybackController::onChangePlaybackRegionAction);

    dispatcher()->reg(this, "toggle-loop-region", this, &PlaybackController::toggleLoopPlayback);
    dispatcher()->reg(this, "clear-loop-region", this, &PlaybackController::clearLoopRegion);
    dispatcher()->reg(this, "set-loop-region-to-selection", this, &PlaybackController::setLoopRegionToSelection);
    dispatcher()->reg(this, "set-selection-to-loop", this, &PlaybackController::setSelectionToLoop);
    dispatcher()->reg(this, "set-loop-region-in-out", this, &PlaybackController::setLoopRegionInOut);
    dispatcher()->reg(this, "toggle-selection-follows-loop-region", this, &PlaybackController::setSelectionFollowsLoopRegion);

    dispatcher()->reg(this, REPEAT_CODE, this, &PlaybackController::togglePlayRepeats);
    dispatcher()->reg(this, PAN_CODE, this, &PlaybackController::toggleAutomaticallyPan);

    dispatcher()->reg(this, PLAYBACK_CHANGE_AUDIO_API_QUERY, this, &PlaybackController::setAudioApi);
    dispatcher()->reg(this, PLAYBACK_CHANGE_PLAYBACK_DEVICE_QUERY, this, &PlaybackController::setAudioOutputDevice);
    dispatcher()->reg(this, PLAYBACK_CHANGE_RECORDING_DEVICE_QUERY, this, &PlaybackController::setAudioInputDevice);
    dispatcher()->reg(this, PLAYBACK_CHANGE_INPUT_CHANNELS_QUERY, this, &PlaybackController::setInputChannels);

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

    selectionController()->clipsSelected().onReceive(this, [this](const trackedit::ClipKeyList& clipKeyList) {
        if (clipKeyList.empty()) {
            return;
        }
        if (!isPlaying()) {
            player()->stop();
            PlaybackRegion selectionRegion = selectionPlaybackRegion();
            if (selectionRegion.isValid()) {
                doChangePlaybackRegion(selectionRegion);
            }
            doSeek(m_lastPlaybackSeekTime, false);
        }
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
    // clip selection have priority over time selection
    if (selectionController()->selectedClips().size() == 1) {
        secs_t clipStartTime = selectionController()->selectedClipStartTime();
        secs_t clipEndTime = selectionController()->selectedClipEndTime();
        return { clipStartTime, clipEndTime };
    }

    if (selectionController()->timeSelectionIsNotEmpty()) {
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

PlaybackStatus PlaybackController::playbackStatus() const
{
    return player()->playbackStatus();
}

void PlaybackController::seek(const muse::secs_t secs, bool applyIfPlaying)
{
    IF_ASSERT_FAILED(player()) {
        return;
    }

    player()->seek(secs, applyIfPlaying);
}

void PlaybackController::reset()
{
    constexpr bool shouldSeek = true;
    constexpr bool shouldUpdatePlaybackRegion = true;
    stop(shouldSeek, shouldUpdatePlaybackRegion);
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
            constexpr bool shouldSeek = true;
            constexpr bool shouldUpdatePlaybackRegion = true;
            stop(shouldSeek, shouldUpdatePlaybackRegion);
        });

        seek(0.0, false); // TODO: get the previous position from the project data
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
            constexpr bool shouldSeek = true;
            constexpr bool shouldUpdatePlaybackRegion = true;
            stop(shouldSeek, shouldUpdatePlaybackRegion);
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

    if (!ignoreSelection) {
        PlaybackRegion selectionRegion = selectionPlaybackRegion();
        if (selectionRegion.isValid()) {
            doChangePlaybackRegion(selectionRegion);
        } else {
            LOGW() << "playback region is not valid";
            // update the playback region "manually" even when not paused
            // (that's why we aren't using the doChangePlaybackRegion)
            updatePlaybackRegion();
        }
    } else {
        doChangePlaybackRegion({});
        doSeek(m_lastPlaybackSeekTime, false);
    }

    if (!isPlaybackStartPositionValid()) {
        return;
    }

    player()->play();
}

void PlaybackController::playTracksAction(const muse::actions::ActionQuery& q)
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
    constexpr bool shouldSeek = true;
    constexpr bool shouldUpdatePlaybackRegion = true;
    stop(shouldSeek, shouldUpdatePlaybackRegion);

    doSeek(0.0, false);

    selectionController()->resetTimeSelection();
}

void PlaybackController::rewindToEndAction()
{
    //! NOTE: In Audacity 3 we can't rewind while playing
    m_lastPlaybackSeekTime = totalPlayTime();
    m_lastPlaybackRegion = { totalPlayTime(), totalPlayTime() };
    constexpr bool shouldSeek = true;
    constexpr bool shouldUpdatePlaybackRegion = true;
    stop(shouldSeek, shouldUpdatePlaybackRegion);

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

    const muse::secs_t secs = q.param("seekTime").toDouble();
    const bool triggerPlay = q.param("triggerPlay").toBool();

    if (isPaused()) {
        player()->stop();
    }

    doSeek(secs, triggerPlay);

    if (triggerPlay) {
        if (isPlaying()) {
            return;
        }

        if (!isPlaybackStartPositionValid()) {
            return;
        }

        player()->play();
    }
}

void PlaybackController::doSeek(const muse::secs_t secs, bool applyIfPlaying)
{
    seek(secs, applyIfPlaying);
    m_lastPlaybackSeekTime = secs;
    m_lastPlaybackRegion = {};
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
    m_lastPlaybackRegion = region;

    if (isStopped()) {
        updatePlaybackRegion();
    }

    if (region.isValid()) {
        m_lastPlaybackSeekTime = m_lastPlaybackRegion.start;
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

    player()->pause();
}

void PlaybackController::stopAction(const muse::actions::ActionQuery& q)
{
    IF_ASSERT_FAILED(q.contains("shouldSeek")) {
        return;
    }
    IF_ASSERT_FAILED(q.contains("shouldUpdatePlaybackRegion")) {
        return;
    }

    const bool shouldSeek = q.param("shouldSeek").toBool();
    const bool shouldUpdatePlaybackRegion = q.param("shouldUpdatePlaybackRegion").toBool();
    stop(shouldSeek, shouldUpdatePlaybackRegion);
}

void PlaybackController::stop(const bool shouldSeek, const bool shouldUpdatePlaybackRegion)
{
    IF_ASSERT_FAILED(player()) {
        return;
    }

    player()->stop();

    // FIXME: those two boolean are temporary, the idea is to revisit the calls to PlaybackController::stop from
    // everywhere and check if we want to keep the seek and the updatePlaybackRegion extra operation
    // because ideally we should be able to call stop() without any extra operation
    if (shouldSeek) {
        seek(m_lastPlaybackSeekTime, false);
    }
    if (shouldUpdatePlaybackRegion) {
        updatePlaybackRegion();
    }
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

    if (selectionController()->timeSelectionIsNotEmpty()) {
        start = selectionController()->dataSelectedStartTime();
        end = selectionController()->dataSelectedEndTime();
    } else if (selectionController()->hasSelectedClips()) {
        start = selectionController()->leftMostSelectedClipStartTime();
        end = selectionController()->rightMostSelectedClipEndTime();
    } else {
        player()->clearLoopRegion();
        return;
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

    int index = q.param("api_index").toInt();

    audioDevicesProvider()->setAudioApi(audioDevicesProvider()->audioApiList().at(index));
}

void PlaybackController::setAudioOutputDevice(const muse::actions::ActionQuery& q)
{
    IF_ASSERT_FAILED(q.contains("device_index")) {
        return;
    }

    int index = q.param("device_index").toInt();

    audioDevicesProvider()->setAudioOutputDevice(audioDevicesProvider()->audioOutputDevices().at(index));
}

void PlaybackController::setAudioInputDevice(const muse::actions::ActionQuery& q)
{
    IF_ASSERT_FAILED(q.contains("device_index")) {
        return;
    }

    int index = q.param("device_index").toInt();

    audioDevicesProvider()->setAudioInputDevice(audioDevicesProvider()->audioInputDevices().at(index));
}

void PlaybackController::setInputChannels(const muse::actions::ActionQuery& q)
{
    IF_ASSERT_FAILED(q.contains("input-channels_index")) {
        return;
    }

    int index = q.param("input-channels_index").toInt();

    audioDevicesProvider()->setInputChannels(audioDevicesProvider()->inputChannelsList().at(index));
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

    if (m_lastPlaybackSeekTime >= totalPlayTime) {
        return false;
    }

    if (m_lastPlaybackRegion.start >= totalPlayTime) {
        return false;
    }

    return true;
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

    if (code == PLAYBACK_PLAY_CODE.toString()) {
        return !recordController()->isRecording();
    }

    if (code == PLAYBACK_REWIND_START_CODE.toString() || code == PLAYBACK_REWIND_END_CODE.toString()) {
        return !isPlaying() && !recordController()->isRecording();
    }

    return true;
}
