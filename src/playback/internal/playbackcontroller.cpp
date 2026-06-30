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

void PlaybackController::init()
{
    m_player = playback()->player();

    dispatcher()->reg(this, PLAYBACK_PLAY_QUERY, this, &PlaybackController::togglePlayAction);
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

    m_player->loopRegionChanged().onNotify(this, [this](){
        m_actionCheckedChanged.send("toggle-loop-region");
    });

    playbackConfiguration()->selectionFollowsLoopRegionChanged().onNotify(this, [this]() {
        m_actionCheckedChanged.send("toggle-selection-follows-loop-region");
    });
}

void PlaybackController::deinit()
{
}

bool PlaybackController::isPlayAllowed() const
{
    return m_player->isPlayAllowed();
}

Notification PlaybackController::isPlayAllowedChanged() const
{
    return m_player->isPlayAllowedChanged();
}

bool PlaybackController::isPlaying() const
{
    return m_player->isPlaying();
}

bool PlaybackController::isPaused() const
{
    return m_player->isPaused();
}

bool PlaybackController::isStopped() const
{
    return m_player->isStopped();
}

bool PlaybackController::isLoopRegionActive() const
{
    return m_player->isLoopRegionActive();
}

Notification PlaybackController::isPlayingChanged() const
{
    return m_player->isPlayingChanged();
}

muse::secs_t PlaybackController::lastPlaybackSeekTime() const
{
    return m_player->lastPlaybackSeekTime();
}

muse::async::Notification PlaybackController::lastPlaybackSeekTimeChanged() const
{
    return m_player->lastPlaybackSeekTimeChanged();
}

PlaybackStatus PlaybackController::playbackStatus() const
{
    return m_player->playbackStatus();
}

void PlaybackController::stopSeekAndUpdatePlaybackRegion()
{
    m_player->stopSeekAndUpdatePlaybackRegion();
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

void PlaybackController::togglePlayAction()
{
    const bool isShiftPressed = application()->keyboardModifiers().testFlag(Qt::ShiftModifier);
    m_player->togglePlay(isShiftPressed /* ignoreSelection */);
}

void PlaybackController::playTracksAction(const muse::actions::ActionQuery&)
{
    // Not implemented yet.
}

void PlaybackController::rewindToStartAction()
{
    m_player->rewindToStart();
}

void PlaybackController::rewindToEndAction()
{
    m_player->rewindToEnd();
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

    m_player->seekTo(secs, triggerPlay);
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

    m_player->changePlaybackRegion(start, end);
}

void PlaybackController::pauseAction()
{
    m_player->pause();
}

void PlaybackController::stopAction()
{
    m_player->stopSeekAndUpdatePlaybackRegion();
}

void PlaybackController::stop()
{
    m_player->stop();
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
    m_player->toggleLoopPlayback();
    notifyActionCheckedChanged("toggle-loop-region");
}

PlaybackRegion PlaybackController::loopRegion() const
{
    return m_player->loopRegion();
}

void PlaybackController::setLoopRegion(const PlaybackRegion& region)
{
    m_player->setLoopRegion(region);
}

void PlaybackController::setLoopRegionStart(const muse::secs_t time)
{
    m_player->setLoopRegionStart(time);
}

void PlaybackController::setLoopRegionEnd(const muse::secs_t time)
{
    m_player->setLoopRegionEnd(time);
}

void PlaybackController::setLoopRegionActive(const bool active)
{
    m_player->setLoopRegionActive(active);
}

void PlaybackController::clearLoopRegion()
{
    m_player->clearLoopRegion();
}

void PlaybackController::setLastPlaybackSeekTime(muse::secs_t secs)
{
    m_player->setLastPlaybackSeekTime(secs);
}

void PlaybackController::loopEditingBegin()
{
    m_player->loopEditingBegin();
}

void PlaybackController::loopEditingEnd()
{
    m_player->loopEditingEnd();
}

bool PlaybackController::isLoopRegionClear() const
{
    return m_player->isLoopRegionClear();
}

muse::async::Notification PlaybackController::loopRegionChanged() const
{
    return m_player->loopRegionChanged();
}

void PlaybackController::setLoopRegionToSelection()
{
    m_player->setLoopRegionToSelection();
}

void PlaybackController::setSelectionToLoop()
{
    m_player->setSelectionToLoop();
}

void PlaybackController::setLoopRegionInOut()
{
    m_player->setLoopRegionInOut();
}

void PlaybackController::setSelectionFollowsLoopRegion()
{
    m_player->setSelectionFollowsLoopRegion();
}

void PlaybackController::setAudioApi(const muse::actions::ActionQuery& q)
{
    IF_ASSERT_FAILED(q.contains("api_index")) {
        return;
    }

    int index = q.param("api_index").toInt();

    audioDevicesProvider()->setApi(audioDevicesProvider()->apis().at(index));
}

void PlaybackController::setAudioOutputDevice(const muse::actions::ActionQuery& q)
{
    IF_ASSERT_FAILED(q.contains("device_index")) {
        return;
    }

    int index = q.param("device_index").toInt();

    audioDevicesProvider()->setOutputDevice(audioDevicesProvider()->outputDevices().at(index));
}

void PlaybackController::setAudioInputDevice(const muse::actions::ActionQuery& q)
{
    IF_ASSERT_FAILED(q.contains("device_index")) {
        return;
    }

    int index = q.param("device_index").toInt();

    audioDevicesProvider()->setInputDevice(audioDevicesProvider()->inputDevices().at(index));
}

void PlaybackController::setInputChannels(const muse::actions::ActionQuery& q)
{
    IF_ASSERT_FAILED(q.contains("input-channels_index")) {
        return;
    }

    int index = q.param("input-channels_index").toInt();

    audioDevicesProvider()->setInputChannels(index);
}

void PlaybackController::rescanAudioDevices()
{
    audioDevicesProvider()->rescan();
}

void PlaybackController::notifyActionCheckedChanged(const ActionCode& actionCode)
{
    m_actionCheckedChanged.send(actionCode);
}

bool PlaybackController::actionChecked(const ActionCode& actionCode) const
{
    QMap<std::string, bool> isChecked {
        { "toggle-loop-region", m_player->isLoopRegionActive() },
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
    return m_player->totalPlayTime();
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
        return !m_player->isPlaying() && !recordController()->isRecording();
    }

    return true;
}
