/*
* Audacity: A Digital Audio Editor
*/
#include "playbackcontroller.h"

#include "playback/iplayback.h"
#include "framework/global/log.h"

using namespace muse;
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

void PlaybackController::togglePlayAction()
{
    const bool isShiftPressed = application()->keyboardModifiers().testFlag(Qt::ShiftModifier);
    transport()->togglePlay(isShiftPressed /* ignoreSelection */);
}

void PlaybackController::playTracksAction(const muse::actions::ActionQuery&)
{
    // Not implemented yet.
}

void PlaybackController::pauseAction()
{
    transport()->pause();
}

void PlaybackController::stopAction()
{
    transport()->stopSeekAndUpdatePlaybackRegion();
}

void PlaybackController::rewindToStartAction()
{
    transport()->rewindToStart();
}

void PlaybackController::rewindToEndAction()
{
    transport()->rewindToEnd();
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

    transport()->seekTo(secs, triggerPlay);
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

    transport()->changePlaybackRegion(start, end);
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
    transport()->toggleLoopPlayback();
    notifyActionCheckedChanged("toggle-loop-region");
}

void PlaybackController::clearLoopRegion()
{
    m_player->clearLoopRegion();
}

void PlaybackController::setLoopRegionToSelection()
{
    transport()->setLoopRegionToSelection();
}

void PlaybackController::setSelectionToLoop()
{
    transport()->setSelectionToLoop();
}

void PlaybackController::setLoopRegionInOut()
{
    transport()->setLoopRegionInOut();
}

void PlaybackController::setSelectionFollowsLoopRegion()
{
    transport()->setSelectionFollowsLoopRegion();
}

void PlaybackController::setAudioApi(const muse::actions::ActionQuery& q)
{
    changeAudioDeviceFromQuery(q, "api_index", audioDevicesProvider()->apis(), [this](const std::string& api) {
        transport()->setAudioApi(api);
    });
}

void PlaybackController::setAudioOutputDevice(const muse::actions::ActionQuery& q)
{
    changeAudioDeviceFromQuery(q, "device_index", audioDevicesProvider()->outputDevices(), [this](const std::string& device) {
        transport()->setAudioOutputDevice(device);
    });
}

void PlaybackController::setAudioInputDevice(const muse::actions::ActionQuery& q)
{
    changeAudioDeviceFromQuery(q, "device_index", audioDevicesProvider()->inputDevices(), [this](const std::string& device) {
        transport()->setAudioInputDevice(device);
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

void PlaybackController::setInputChannels(const muse::actions::ActionQuery& q)
{
    IF_ASSERT_FAILED(q.contains("input-channels_index")) {
        return;
    }

    transport()->setInputChannels(q.param("input-channels_index").toInt());
}

void PlaybackController::rescanAudioDevices()
{
    transport()->rescanAudioDevices();
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
