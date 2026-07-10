/*
* Audacity: A Digital Audio Editor
*/
#include "transportactionscontroller.h"

#include "playback/iplayer.h"
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

void TransportActionsController::init()
{
    dispatcher()->reg(this, PLAYBACK_PLAY_QUERY, this, &TransportActionsController::togglePlayAction);
    dispatcher()->reg(this, PLAYBACK_PLAY_TRACKS_QUERY, this, &TransportActionsController::playTracksAction);
    dispatcher()->reg(this, PLAYBACK_PAUSE_QUERY, this, &TransportActionsController::pauseAction);
    dispatcher()->reg(this, PLAYBACK_STOP_QUERY, this, &TransportActionsController::stopAction);
    dispatcher()->reg(this, PLAYBACK_REWIND_START_QUERY, this, &TransportActionsController::rewindToStartAction);
    dispatcher()->reg(this, PLAYBACK_REWIND_END_QUERY, this, &TransportActionsController::rewindToEndAction);
    dispatcher()->reg(this, PLAYBACK_SEEK_QUERY, this, &TransportActionsController::onSeekAction);
    dispatcher()->reg(this, PLAYBACK_CHANGE_PLAY_REGION_QUERY, this, &TransportActionsController::onChangePlaybackRegionAction);
    dispatcher()->reg(this, PLAYBACK_CHANGE_AUDIO_API_QUERY, this, &TransportActionsController::setAudioApi);
    dispatcher()->reg(this, PLAYBACK_CHANGE_PLAYBACK_DEVICE_QUERY, this, &TransportActionsController::setAudioOutputDevice);
    dispatcher()->reg(this, PLAYBACK_CHANGE_RECORDING_DEVICE_QUERY, this, &TransportActionsController::setAudioInputDevice);
    dispatcher()->reg(this, PLAYBACK_CHANGE_INPUT_CHANNELS_QUERY, this, &TransportActionsController::setInputChannels);

    dispatcher()->reg(this, REPEAT_CODE, this, &TransportActionsController::togglePlayRepeats);
    dispatcher()->reg(this, PAN_CODE, this, &TransportActionsController::toggleAutomaticallyPan);

    dispatcher()->reg(this, "toggle-loop-region", this, &TransportActionsController::toggleLoopPlayback);
    dispatcher()->reg(this, "clear-loop-region", this, &TransportActionsController::clearLoopRegion);
    dispatcher()->reg(this, "set-loop-region-to-selection", this, &TransportActionsController::setLoopRegionToSelection);
    dispatcher()->reg(this, "set-selection-to-loop", this, &TransportActionsController::setSelectionToLoop);
    dispatcher()->reg(this, "set-loop-region-in-out", this, &TransportActionsController::setLoopRegionInOut);
    dispatcher()->reg(this, "toggle-selection-follows-loop-region", this, &TransportActionsController::setSelectionFollowsLoopRegion);

    dispatcher()->reg(this, "rescan-devices", this, &TransportActionsController::rescanAudioDevices);

    player()->loopRegionChanged().onNotify(this, [this](){
        m_actionCheckedChanged.send("toggle-loop-region");
    });

    playbackConfiguration()->selectionFollowsLoopRegionChanged().onNotify(this, [this]() {
        m_actionCheckedChanged.send("toggle-selection-follows-loop-region");
    });
}

void TransportActionsController::deinit()
{
}

void TransportActionsController::togglePlayAction()
{
    const bool isShiftPressed = application()->keyboardModifiers().testFlag(Qt::ShiftModifier);
    transport()->togglePlay(isShiftPressed /* ignoreSelection */);
}

void TransportActionsController::playTracksAction(const muse::actions::ActionQuery&)
{
    // Not implemented yet.
}

void TransportActionsController::pauseAction()
{
    transport()->pause();
}

void TransportActionsController::stopAction()
{
    transport()->stopSeekAndUpdatePlaybackRegion();
}

void TransportActionsController::rewindToStartAction()
{
    transport()->rewindToStart();
}

void TransportActionsController::rewindToEndAction()
{
    transport()->rewindToEnd();
}

void TransportActionsController::onSeekAction(const muse::actions::ActionQuery& q)
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

void TransportActionsController::onChangePlaybackRegionAction(const muse::actions::ActionQuery& q)
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

void TransportActionsController::togglePlayRepeats()
{
    NOT_IMPLEMENTED;

    // configuration()->setIsPlayRepeatsEnabled(!playRepeatsEnabled);

    notifyActionCheckedChanged(REPEAT_CODE);
}

void TransportActionsController::toggleAutomaticallyPan()
{
    NOT_IMPLEMENTED;

    // configuration()->setIsAutomaticallyPanEnabled(!panEnabled);

    notifyActionCheckedChanged(PAN_CODE);
}

void TransportActionsController::toggleLoopPlayback()
{
    transport()->toggleLoopPlayback();
    notifyActionCheckedChanged("toggle-loop-region");
}

void TransportActionsController::clearLoopRegion()
{
    player()->clearLoopRegion();
}

void TransportActionsController::setLoopRegionToSelection()
{
    transport()->setLoopRegionToSelection();
}

void TransportActionsController::setSelectionToLoop()
{
    transport()->setSelectionToLoop();
}

void TransportActionsController::setLoopRegionInOut()
{
    transport()->setLoopRegionInOut();
}

void TransportActionsController::setSelectionFollowsLoopRegion()
{
    transport()->setSelectionFollowsLoopRegion();
}

void TransportActionsController::setAudioApi(const muse::actions::ActionQuery& q)
{
    changeAudioDeviceFromQuery(q, "api_index", audioDevicesProvider()->apis(), [this](const std::string& api) {
        transport()->setAudioApi(api);
    });
}

void TransportActionsController::setAudioOutputDevice(const muse::actions::ActionQuery& q)
{
    changeAudioDeviceFromQuery(q, "device_index", audioDevicesProvider()->outputDevices(), [this](const std::string& device) {
        transport()->setAudioOutputDevice(device);
    });
}

void TransportActionsController::setAudioInputDevice(const muse::actions::ActionQuery& q)
{
    changeAudioDeviceFromQuery(q, "device_index", audioDevicesProvider()->inputDevices(), [this](const std::string& device) {
        transport()->setAudioInputDevice(device);
    });
}

void TransportActionsController::changeAudioDeviceFromQuery(const muse::actions::ActionQuery& q, const std::string& indexParam,
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

void TransportActionsController::setInputChannels(const muse::actions::ActionQuery& q)
{
    IF_ASSERT_FAILED(q.contains("input-channels_count")) {
        return;
    }

    transport()->setInputChannels(q.param("input-channels_count").toInt());
}

void TransportActionsController::rescanAudioDevices()
{
    transport()->rescanAudioDevices();
}

void TransportActionsController::notifyActionCheckedChanged(const ActionCode& actionCode)
{
    m_actionCheckedChanged.send(actionCode);
}

bool TransportActionsController::actionChecked(const ActionCode& actionCode) const
{
    QMap<std::string, bool> isChecked {
        { "toggle-loop-region", player()->isLoopRegionActive() },
        { "toggle-selection-follows-loop-region", playbackConfiguration()->selectionFollowsLoopRegion() }
    };

    return isChecked[actionCode];
}

Channel<ActionCode> TransportActionsController::actionCheckedChanged() const
{
    return m_actionCheckedChanged;
}

bool TransportActionsController::canReceiveAction(const ActionCode& code) const
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
        return !player()->isPlaying() && !recordController()->isRecording();
    }

    return true;
}
