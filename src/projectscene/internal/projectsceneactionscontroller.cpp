/*
* Audacity: A Digital Audio Editor
*/

#include "project/iaudacityproject.h"

#include "framework/rcommand/actiontocommand.h"

#include "projectsceneactionscontroller.h"
#include "../projectscenecommands.h"

using namespace muse;
using namespace au::projectscene;
using namespace muse::async;
using namespace muse::actions;
using namespace muse::rcommand;

static const ActionCode VERTICAL_RULERS_CODE("toggle-vertical-rulers");
static const ActionCode RMS_IN_WAVEFORM_CODE("toggle-rms-in-waveform");
static const ActionCode CLIPPING_IN_WAVEFORM_CODE("toggle-clipping-in-waveform");
static const ActionCode MINUTES_SECONDS_RULER("minutes-seconds-ruler");
static const ActionCode BEATS_MEASURES_RULER("beats-measures-ruler");
static const ActionCode CLIP_PITCH_AND_SPEED_CODE("clip-pitch-speed");
static const ActionCode TOGGLE_UPDATE_DISPLAY_WHILE_PLAYING_CODE("toggle-update-display-while-playing");
static const ActionCode TOGGLE_PINNED_PLAY_HEAD_CODE("toggle-pinned-play-head");
static const ActionCode TOGGLE_PLAYBACK_ON_RULER_CLICK_ENABLED_CODE("toggle-playback-on-ruler-click-enabled");
static const ActionQuery TOGGLE_TRACK_HALF_WAVE("action://projectscene/track-view-half-wave");
static const ActionCode LABEL_OPEN_EDITOR_CODE("open-label-editor");
static const ActionCode CLIP_GAIN_CODE("clip-gain");

static const muse::Uri EDIT_PITCH_AND_SPEED_URI("audacity://projectscene/editpitchandspeed");

namespace {
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

CommandQuery clipKeyConv(const Command& command, const ActionData& args)
{
    CommandQuery query(command);
    if (args.empty()) {
        return query;
    }

    const au::trackedit::ClipKey clipKey = args.arg<au::trackedit::ClipKey>(0);
    query.addParam("trackId", Val(static_cast<int64_t>(clipKey.trackId)));
    query.addParam("clipId", Val(static_cast<int64_t>(clipKey.itemId)));
    return query;
}
}

void ProjectSceneActionsController::init()
{
    auto cd = commandDispatcher();
    cd->onRequest(this, PROJECTSCENE_MINUTES_SECONDS_RULER_COMMAND, [this]() { return toggleMinutesSecondsRuler(); });
    cd->onRequest(this, PROJECTSCENE_BEATS_MEASURES_RULER_COMMAND, [this]() { return toggleBeatsMeasuresRuler(); });
    cd->onRequest(this, PROJECTSCENE_TOGGLE_VERTICAL_RULERS_COMMAND, [this]() { return toggleVerticalRulers(); });
    cd->onRequest(this, PROJECTSCENE_TOGGLE_RMS_IN_WAVEFORM_COMMAND, [this]() { return toggleRMSInWaveform(); });
    cd->onRequest(this, PROJECTSCENE_TOGGLE_CLIPPING_IN_WAVEFORM_COMMAND, [this]() { return toggleClippingInWaveform(); });
    cd->onRequest(this, PROJECTSCENE_TOGGLE_UPDATE_DISPLAY_WHILE_PLAYING_COMMAND, [this]() { return toggleUpdateDisplayWhilePlaying(); });
    cd->onRequest(this, PROJECTSCENE_TOGGLE_PINNED_PLAY_HEAD_COMMAND, [this]() { return togglePinnedPlayHead(); });
    cd->onRequest(this, PROJECTSCENE_TOGGLE_PLAYBACK_ON_RULER_CLICK_COMMAND, [this]() { return togglePlaybackOnRulerClickEnabled(); });
    cd->onRequest(this, PROJECTSCENE_TOGGLE_TRACK_HALF_WAVE_COMMAND, [this](const Params& params) { return toggleTrackHalfWave(params); });
    cd->onRequest(this, PROJECTSCENE_TOGGLE_CLIP_GAIN_AUTOMATION_COMMAND, [this]() { return toggleAutomation(); });
    cd->onRequest(this, PROJECTSCENE_CLIP_PITCH_AND_SPEED_COMMAND, [this](const Params& params) {
        return openClipPitchAndSpeedEdit(params);
    });
    cd->onRequest(this, PROJECTSCENE_OPEN_LABEL_EDITOR_COMMAND, [this]() { return openLabelEditor(); });

    static const std::vector<ActionToCommand> actionToCommand = {
        { MINUTES_SECONDS_RULER, PROJECTSCENE_MINUTES_SECONDS_RULER_COMMAND, {} },
        { BEATS_MEASURES_RULER, PROJECTSCENE_BEATS_MEASURES_RULER_COMMAND, {} },
        { VERTICAL_RULERS_CODE, PROJECTSCENE_TOGGLE_VERTICAL_RULERS_COMMAND, {} },
        { RMS_IN_WAVEFORM_CODE, PROJECTSCENE_TOGGLE_RMS_IN_WAVEFORM_COMMAND, {} },
        { CLIPPING_IN_WAVEFORM_CODE, PROJECTSCENE_TOGGLE_CLIPPING_IN_WAVEFORM_COMMAND, {} },
        { TOGGLE_UPDATE_DISPLAY_WHILE_PLAYING_CODE, PROJECTSCENE_TOGGLE_UPDATE_DISPLAY_WHILE_PLAYING_COMMAND, {} },
        { TOGGLE_PINNED_PLAY_HEAD_CODE, PROJECTSCENE_TOGGLE_PINNED_PLAY_HEAD_COMMAND, {} },
        { TOGGLE_PLAYBACK_ON_RULER_CLICK_ENABLED_CODE, PROJECTSCENE_TOGGLE_PLAYBACK_ON_RULER_CLICK_COMMAND, {} },
        { TOGGLE_TRACK_HALF_WAVE.toString(), PROJECTSCENE_TOGGLE_TRACK_HALF_WAVE_COMMAND, queryParamsConv },
        { CLIP_GAIN_CODE, PROJECTSCENE_TOGGLE_CLIP_GAIN_AUTOMATION_COMMAND, {} },
        { CLIP_PITCH_AND_SPEED_CODE, PROJECTSCENE_CLIP_PITCH_AND_SPEED_COMMAND, clipKeyConv },
        { LABEL_OPEN_EDITOR_CODE, PROJECTSCENE_OPEN_LABEL_EDITOR_COMMAND, {} },
    };
    registerActionToCommand(this, actionToCommand, commandDispatcher(), dispatcher());

    projectSceneUiState()->timelineRulerModeChanged().onNotify(this, [this]() {
        notifyActionCheckedChanged(MINUTES_SECONDS_RULER);
        notifyActionCheckedChanged(BEATS_MEASURES_RULER);
    });
}

void ProjectSceneActionsController::notifyActionCheckedChanged(const ActionCode& actionCode)
{
    m_actionCheckedChanged.send(actionCode);
}

muse::Ret ProjectSceneActionsController::toggleMinutesSecondsRuler()
{
    projectSceneUiState()->setTimelineRulerMode(TimelineRulerMode::MINUTES_AND_SECONDS);
    return make_ok();
}

muse::Ret ProjectSceneActionsController::toggleBeatsMeasuresRuler()
{
    projectSceneUiState()->setTimelineRulerMode(TimelineRulerMode::BEATS_AND_MEASURES);
    return make_ok();
}

muse::Ret ProjectSceneActionsController::toggleVerticalRulers()
{
    bool verticalRulersVisible = configuration()->isVerticalRulersVisible();
    configuration()->setVerticalRulersVisible(!verticalRulersVisible);
    notifyActionCheckedChanged(VERTICAL_RULERS_CODE);
    return make_ok();
}

muse::Ret ProjectSceneActionsController::toggleRMSInWaveform()
{
    bool rmsVisible = configuration()->isRMSInWaveformVisible();
    configuration()->setRMSInWaveformVisible(!rmsVisible);
    notifyActionCheckedChanged(RMS_IN_WAVEFORM_CODE);
    return make_ok();
}

muse::Ret ProjectSceneActionsController::toggleClippingInWaveform()
{
    bool clippingVisible = configuration()->isClippingInWaveformVisible();
    configuration()->setClippingInWaveformVisible(!clippingVisible);
    notifyActionCheckedChanged(CLIPPING_IN_WAVEFORM_CODE);
    return make_ok();
}

muse::Ret ProjectSceneActionsController::toggleUpdateDisplayWhilePlaying()
{
    bool enabled = configuration()->updateDisplayWhilePlayingEnabled();
    configuration()->setUpdateDisplayWhilePlayingEnabled(!enabled);
    notifyActionCheckedChanged(TOGGLE_UPDATE_DISPLAY_WHILE_PLAYING_CODE);
    return make_ok();
}

muse::Ret ProjectSceneActionsController::togglePinnedPlayHead()
{
    bool enabled = configuration()->pinnedPlayHeadEnabled();
    configuration()->setPinnedPlayHeadEnabled(!enabled);
    notifyActionCheckedChanged(TOGGLE_PINNED_PLAY_HEAD_CODE);
    return make_ok();
}

muse::Ret ProjectSceneActionsController::openClipPitchAndSpeedEdit(const Params& params)
{
    if (interactive()->isOpened(EDIT_PITCH_AND_SPEED_URI).val) {
        return make_ret(Ret::Code::Busy);
    }

    IF_ASSERT_FAILED(params.contains("trackId") && params.contains("clipId")) {
        return make_ret(Ret::Code::BadArgs);
    }

    const trackedit::ClipKey clipKey(params.at("trackId").toInt64(), params.at("clipId").toInt64());
    if (!clipKey.isValid()) {
        return make_ret(Ret::Code::BadArgs);
    }

    muse::UriQuery query(EDIT_PITCH_AND_SPEED_URI);
    query.addParam("trackId", muse::Val(std::to_string(clipKey.trackId)));
    query.addParam("clipId", muse::Val(std::to_string(clipKey.itemId)));
    query.addParam("focusItemName", muse::Val("pitch"));

    interactive()->open(query);
    return make_ok();
}

muse::Ret ProjectSceneActionsController::openLabelEditor()
{
    interactive()->open("audacity://projectscene/openlabeleditor");
    return make_ok();
}

muse::Ret ProjectSceneActionsController::togglePlaybackOnRulerClickEnabled()
{
    bool isEnabled = configuration()->playbackOnRulerClickEnabled();
    configuration()->setPlaybackOnRulerClickEnabled(!isEnabled);
    notifyActionCheckedChanged(TOGGLE_PLAYBACK_ON_RULER_CLICK_ENABLED_CODE);
    return make_ok();
}

muse::Ret ProjectSceneActionsController::toggleAutomation()
{
    project::IAudacityProjectPtr prj = globalContext()->currentProject();
    if (!prj) {
        return make_ret(Ret::Code::NotSupported);
    }

    const auto viewState = prj->viewState();
    if (viewState == nullptr) {
        return make_ret(Ret::Code::NotSupported);
    }

    const bool automationState = viewState->clipGainAutomationEnabled().val;
    const bool enablingAutomation = !automationState;
    if (enablingAutomation && viewState->globalSpectrogramToggleIsOn()) {
        viewState->toggleGlobalSpectrogramView();
    }

    viewState->setClipGainAutomationEnabled(enablingAutomation);
    return make_ok();
}

muse::Ret ProjectSceneActionsController::toggleTrackHalfWave(const Params& params)
{
    IF_ASSERT_FAILED(params.contains("trackId")) {
        return make_ret(Ret::Code::BadArgs);
    }
    const int trackId = params.at("trackId").toInt();

    project::IAudacityProjectPtr prj = globalContext()->currentProject();
    if (!prj) {
        return make_ret(Ret::Code::NotSupported);
    }

    const auto viewState = prj->viewState();
    if (viewState == nullptr) {
        return make_ret(Ret::Code::NotSupported);
    }

    viewState->toggleHalfWave(trackId);
    notifyActionCheckedChanged(TOGGLE_TRACK_HALF_WAVE.toString());
    return make_ok();
}

bool ProjectSceneActionsController::actionChecked(const ActionCode& actionCode) const
{
    QMap<std::string, bool> isChecked {
        { VERTICAL_RULERS_CODE, configuration()->isVerticalRulersVisible() },
        { RMS_IN_WAVEFORM_CODE, configuration()->isRMSInWaveformVisible() },
        { CLIPPING_IN_WAVEFORM_CODE, configuration()->isClippingInWaveformVisible() },
        { MINUTES_SECONDS_RULER, projectSceneUiState()->timelineRulerMode() == TimelineRulerMode::MINUTES_AND_SECONDS },
        { BEATS_MEASURES_RULER, projectSceneUiState()->timelineRulerMode() == TimelineRulerMode::BEATS_AND_MEASURES },
        { TOGGLE_PLAYBACK_ON_RULER_CLICK_ENABLED_CODE, configuration()->playbackOnRulerClickEnabled() },
        { TOGGLE_UPDATE_DISPLAY_WHILE_PLAYING_CODE, configuration()->updateDisplayWhilePlayingEnabled() },
        { TOGGLE_PINNED_PLAY_HEAD_CODE, configuration()->pinnedPlayHeadEnabled() }
    };

    return isChecked[actionCode];
}

Channel<ActionCode> ProjectSceneActionsController::actionCheckedChanged() const
{
    return m_actionCheckedChanged;
}

bool ProjectSceneActionsController::canReceiveAction(const ActionCode&) const
{
    return globalContext()->currentProject() != nullptr;
}

Channel<ActionCode> ProjectSceneActionsController::actionEnabledChanged() const
{
    return m_actionEnabledChanged;
}
