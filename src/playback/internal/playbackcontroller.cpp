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

static const ActionCode PLAY_CODE("play");
static const ActionCode PAUSE_CODE("pause");
static const ActionCode STOP_CODE("stop");
static const ActionCode REWIND_START_CODE("rewind-start");
static const ActionCode REWIND_END_CODE("rewind-end");
static const ActionCode SEEK_CODE("playback-seek");
static const ActionCode CHANGE_PLAY_REGION_CODE("playback-play-region-change");
static const ActionCode PAN_CODE("pan");
static const ActionCode REPEAT_CODE("repeat");

static const secs_t TIME_EPS = secs_t(1 / 1000.0);

static const ActionQuery CHANGE_AUDIO_API_QUERY("action://playback/change-api");
static const ActionQuery CHANGE_PLAYBACK_DEVICE_QUERY("action://playback/change-playback-device");
static const ActionQuery CHANGE_RECORDING_DEVICE_QUERY("action://playback/change-recording-device");
static const ActionQuery CHANGE_INPUT_CHANNELS_QUERY("action://playback/change-input-channels");

void PlaybackController::init()
{
    dispatcher()->reg(this, PLAY_CODE, this, &PlaybackController::togglePlay);
    dispatcher()->reg(this, PAUSE_CODE, this, &PlaybackController::pause);
    dispatcher()->reg(this, STOP_CODE, this, &PlaybackController::stop);
    dispatcher()->reg(this, REWIND_START_CODE, this, &PlaybackController::rewindToStart);
    dispatcher()->reg(this, REWIND_END_CODE, this, &PlaybackController::rewindToEnd);
    dispatcher()->reg(this, SEEK_CODE, this, &PlaybackController::onSeekAction);
    dispatcher()->reg(this, CHANGE_PLAY_REGION_CODE, this, &PlaybackController::onChangePlaybackRegionAction);

    dispatcher()->reg(this, "toggle-loop-region", this, &PlaybackController::toggleLoopPlayback);
    dispatcher()->reg(this, "clear-loop-region", this, &PlaybackController::clearLoopRegion);
    dispatcher()->reg(this, "set-loop-region-to-selection", this, &PlaybackController::setLoopRegionToSelection);
    dispatcher()->reg(this, "set-selection-to-loop", this, &PlaybackController::setSelectionToLoop);
    dispatcher()->reg(this, "set-loop-region-in-out", this, &PlaybackController::setLoopRegionInOut);
    dispatcher()->reg(this, "toggle-selection-follows-loop-region", this, &PlaybackController::setSelectionFollowsLoopRegion);

    dispatcher()->reg(this, REPEAT_CODE, this, &PlaybackController::togglePlayRepeats);
    dispatcher()->reg(this, PAN_CODE, this, &PlaybackController::toggleAutomaticallyPan);

    dispatcher()->reg(this, CHANGE_AUDIO_API_QUERY, this, &PlaybackController::setAudioApi);
    dispatcher()->reg(this, CHANGE_PLAYBACK_DEVICE_QUERY, this, &PlaybackController::setAudioOutputDevice);
    dispatcher()->reg(this, CHANGE_RECORDING_DEVICE_QUERY, this, &PlaybackController::setAudioInputDevice);
    dispatcher()->reg(this, CHANGE_INPUT_CHANNELS_QUERY, this, &PlaybackController::setInputChannels);

    globalContext()->currentProjectChanged().onNotify(this, [this]() {
        onProjectChanged();
    });

    m_player = playback()->player();
    globalContext()->setPlayer(m_player);

    m_player->playbackStatusChanged().onReceive(this, [this](PlaybackStatus) {
        m_isPlayingChanged.notify();
    });

    // No need to assert that we're on the main thread here: this is the init method of a controller...
    m_player->playbackPositionChanged().onReceive(this, [this](const muse::secs_t&) {
        if (isPlaybackPositionOnTheEndOfProject() || isPlaybackPositionOnTheEndOfPlaybackRegion()) {
            //! NOTE: just stop, without seek
            player()->stop();
            if (player()->playbackRegion() != m_lastPlaybackRegion && !isEqualToPlaybackPosition(m_lastPlaybackRegion.end)) {
                // we want to update the playback region in case user made new selection during playback
                updatePlaybackRegion();
            }
        }
    });

    m_player->loopRegionChanged().onNotify(this, [this](){
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
    // this?
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

void PlaybackController::seek(const muse::secs_t secs, bool applyIfPlaying)
{
    IF_ASSERT_FAILED(player()) {
        return;
    }

    player()->seek(secs, applyIfPlaying);
}

void PlaybackController::reset()
{
    stop();
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
            stop();
        });

        seek(0.0, false); // TODO: get the previous position from the project data
    }
}

void PlaybackController::togglePlay()
{
    if (!isPlayAllowed()) {
        LOGW() << "playback not allowed";
        return;
    }

    const bool isShiftPressed = application()->keyboardModifiers().testFlag(Qt::ShiftModifier);
    if (isPlaying()) {
        if (isShiftPressed) {
            stop();
        } else {
            pause();
        }
    } else if (isPaused()) {
        if (isSelectionPlaybackRegionChanged()) {
            //! NOTE: just stop, without seek
            player()->stop();
            play(false);
        } else if (isShiftPressed) {
            //! NOTE: set the current position as start position
            doSeek(m_player->playbackPosition(), false);
            play(true /* ignoreSelection */);
        } else {
            resume();
        }
    } else {
        if (isPlaybackPositionOnTheEndOfProject() || isPlaybackPositionOnTheEndOfPlaybackRegion()) {
            doSeek(0.0, false);
        }

        play(isShiftPressed /* ignoreSelection */);
    }
}

void PlaybackController::play(bool ignoreSelection)
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

void PlaybackController::rewindToStart()
{
    //! NOTE: In Audacity 3 we can't rewind while playing
    stop();

    doSeek(0.0, false);

    selectionController()->resetTimeSelection();
}

void PlaybackController::rewindToEnd()
{
    //! NOTE: In Audacity 3 we can't rewind while playing
    m_lastPlaybackSeekTime = totalPlayTime();
    m_lastPlaybackRegion = { totalPlayTime(), totalPlayTime() };
    stop();

    selectionController()->resetTimeSelection();
}

void PlaybackController::onSeekAction(const muse::actions::ActionData& args)
{
    IF_ASSERT_FAILED(args.count() > 0) {
        return;
    }

    muse::secs_t secs = args.arg<double>(0);
    bool triggerPlay = args.count() > 1 ? args.arg<bool>(1) : false;

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

void PlaybackController::onChangePlaybackRegionAction(const muse::actions::ActionData& args)
{
    IF_ASSERT_FAILED(args.count() > 1) {
        return;
    }

    muse::secs_t start = args.arg<double>(0);
    muse::secs_t end = args.arg<double>(1);

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

void PlaybackController::pause()
{
    IF_ASSERT_FAILED(player()) {
        return;
    }

    player()->pause();
}

void PlaybackController::stop()
{
    IF_ASSERT_FAILED(player()) {
        return;
    }

    player()->stop();

    seek(m_lastPlaybackSeekTime, false);
    updatePlaybackRegion();
}

void PlaybackController::resume()
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
    PlaybackRegion region = m_player->loopRegion();

    muse::UriQuery loopRegionInOutUri("audacity://playback/loop_region_in_out");
    loopRegionInOutUri.addParam("title", muse::Val(muse::trc("trackedit", "Set looping region in/out")));
    loopRegionInOutUri.addParam("start", muse::Val(static_cast<double>(region.start)));
    loopRegionInOutUri.addParam("end", muse::Val(static_cast<double>(region.end)));

    RetVal<Val> rv = interactive()->openSync(loopRegionInOutUri);
    if (!rv.ret.success()) {
        return;
    }

    QVariantMap vals = rv.val.toQVariant().toMap();

    m_player->setLoopRegion({ vals["start"].toDouble(), vals["end"].toDouble() });
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

bool PlaybackController::isEqualToPlaybackPosition(secs_t position) const
{
    secs_t playbackPosition = player()->playbackPosition();
    return playbackPosition - TIME_EPS <= position && position <= playbackPosition + TIME_EPS;
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
    if (globalContext()->currentProject() == nullptr) {
        return false;
    }

    if (code == PLAY_CODE) {
        return !recordController()->isRecording();
    }

    if (code == REWIND_START_CODE || code == REWIND_END_CODE) {
        return !isPlaying() && !recordController()->isRecording();
    }

    return true;
}
