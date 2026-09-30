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
static const ActionCode ZOOM_IN_CODE("zoom-in");
static const ActionCode ZOOM_OUT_CODE("zoom-out");
static const ActionCode ZOOM_DEFAULT_CODE("zoom-default");
static const ActionCode ZOOM_TO_SELECTION_CODE("zoom-to-selection");
static const ActionCode ZOOM_TO_FIT_PROJECT_CODE("zoom-to-fit-project");
static const ActionCode ZOOM_TOGGLE_CODE("zoom-toggle");
static const ActionCode CENTER_VIEW_ON_PLAYHEAD_CODE("center-view-on-playhead");
static const ActionCode TIMELINE_CONTEXT_MENU_CODE("timeline-context-menu");
static const ActionCode PLAY_POSITION_DECREASE_CODE("play-position-decrease");
static const ActionCode PLAY_POSITION_INCREASE_CODE("play-position-increase");
static const ActionCode SEL_EXT_LEFT_CODE("sel-ext-left");
static const ActionCode SEL_EXT_RIGHT_CODE("sel-ext-right");
static const ActionCode SEL_CNTR_LEFT_CODE("sel-cntr-left");
static const ActionCode SEL_CNTR_RIGHT_CODE("sel-cntr-right");
static const ActionCode CURS_SEL_START_CODE("curs-sel-start");
static const ActionCode CURS_SEL_END_CODE("curs-sel-end");
static const ActionCode TOGGLE_EFFECTS_CODE("toggle-effects");
static const ActionCode ADD_REALTIME_EFFECTS_CODE("add-realtime-effects");
static const ActionCode AUDIO_SETUP_CODE("audio-setup");
static const ActionCode GET_EFFECTS_CODE("get-effects");

static const muse::Uri GET_EFFECTS_URI("audacity://projectscene/geteffects");

static const muse::Uri EDIT_PITCH_AND_SPEED_URI("audacity://projectscene/editpitchandspeed");

static const std::string ONLY_IF_PLAYHEAD_NOT_VISIBLE_PARAM("only_if_playhead_not_visible");

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

CommandQuery centerViewOnPlayheadConv(const Command& command, const ActionData& args)
{
    CommandQuery query(command);
    if (args.empty()) {
        return query;
    }

    query.addParam(ONLY_IF_PLAYHEAD_NOT_VISIBLE_PARAM, Val(args.arg<bool>(0)));
    return query;
}
}

void ProjectSceneActionsController::init()
{
    auto cd = commandDispatcher();
    registerViewCommand(PROJECTSCENE_ZOOM_IN_COMMAND, &ProjectSceneActionsController::m_timelineViewController,
                        &ITimelineViewController::zoomIn);
    registerViewCommand(PROJECTSCENE_ZOOM_OUT_COMMAND, &ProjectSceneActionsController::m_timelineViewController,
                        &ITimelineViewController::zoomOut);
    registerViewCommand(PROJECTSCENE_ZOOM_DEFAULT_COMMAND, &ProjectSceneActionsController::m_timelineViewController,
                        &ITimelineViewController::zoomDefault);
    registerViewCommand(PROJECTSCENE_ZOOM_TO_SELECTION_COMMAND, &ProjectSceneActionsController::m_timelineViewController,
                        &ITimelineViewController::fitSelectionToWidth);
    registerViewCommand(PROJECTSCENE_ZOOM_TO_FIT_PROJECT_COMMAND, &ProjectSceneActionsController::m_timelineViewController,
                        &ITimelineViewController::fitProjectToWidth);
    registerViewCommand(PROJECTSCENE_ZOOM_TOGGLE_COMMAND, &ProjectSceneActionsController::m_timelineViewController,
                        &ITimelineViewController::zoomToggle);
    cd->onRequest(this, PROJECTSCENE_TIMELINE_CONTEXT_MENU_COMMAND, [this]() {
        m_timelineContextMenuRequested.notify();
        return make_ok();
    });
    cd->onRequest(this, PROJECTSCENE_CENTER_VIEW_ON_PLAYHEAD_COMMAND, [this](const Params& params) {
        return centerViewOnPlayhead(params);
    });

    registerViewCommand(PROJECTSCENE_PLAY_POSITION_DECREASE_COMMAND, &ProjectSceneActionsController::m_playPositionViewController,
                        &IPlayPositionViewController::playPositionDecrease);
    registerViewCommand(PROJECTSCENE_PLAY_POSITION_INCREASE_COMMAND, &ProjectSceneActionsController::m_playPositionViewController,
                        &IPlayPositionViewController::playPositionIncrease);
    registerViewCommand(PROJECTSCENE_SELECTION_EXTEND_LEFT_COMMAND, &ProjectSceneActionsController::m_playPositionViewController,
                        &IPlayPositionViewController::selectionExtendLeft);
    registerViewCommand(PROJECTSCENE_SELECTION_EXTEND_RIGHT_COMMAND, &ProjectSceneActionsController::m_playPositionViewController,
                        &IPlayPositionViewController::selectionExtendRight);
    registerViewCommand(PROJECTSCENE_SELECTION_CONTRACT_LEFT_COMMAND, &ProjectSceneActionsController::m_playPositionViewController,
                        &IPlayPositionViewController::selectionContractLeft);
    registerViewCommand(PROJECTSCENE_SELECTION_CONTRACT_RIGHT_COMMAND, &ProjectSceneActionsController::m_playPositionViewController,
                        &IPlayPositionViewController::selectionContractRight);
    registerViewCommand(PROJECTSCENE_CURSOR_TO_SELECTION_START_COMMAND, &ProjectSceneActionsController::m_playPositionViewController,
                        &IPlayPositionViewController::cursorToSelectionStart);
    registerViewCommand(PROJECTSCENE_CURSOR_TO_SELECTION_END_COMMAND, &ProjectSceneActionsController::m_playPositionViewController,
                        &IPlayPositionViewController::cursorToSelectionEnd);

    cd->onRequest(this, PROJECTSCENE_TOGGLE_EFFECTS_PANEL_COMMAND, [this]() { return toggleEffectsPanel(); });
    cd->onRequest(this, PROJECTSCENE_ADD_REALTIME_EFFECTS_COMMAND, [this]() { return toggleEffectsPanel(); });
    cd->onRequest(this, PROJECTSCENE_AUDIO_SETUP_COMMAND, [this]() { return requestAudioSetupContextMenu(); });
    cd->onRequest(this, PROJECTSCENE_GET_EFFECTS_COMMAND, [this]() { return openGetEffectsDialog(); });

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
        { ZOOM_IN_CODE, PROJECTSCENE_ZOOM_IN_COMMAND, {} },
        { ZOOM_OUT_CODE, PROJECTSCENE_ZOOM_OUT_COMMAND, {} },
        { ZOOM_DEFAULT_CODE, PROJECTSCENE_ZOOM_DEFAULT_COMMAND, {} },
        { ZOOM_TO_SELECTION_CODE, PROJECTSCENE_ZOOM_TO_SELECTION_COMMAND, {} },
        { ZOOM_TO_FIT_PROJECT_CODE, PROJECTSCENE_ZOOM_TO_FIT_PROJECT_COMMAND, {} },
        { ZOOM_TOGGLE_CODE, PROJECTSCENE_ZOOM_TOGGLE_COMMAND, {} },
        { CENTER_VIEW_ON_PLAYHEAD_CODE, PROJECTSCENE_CENTER_VIEW_ON_PLAYHEAD_COMMAND, centerViewOnPlayheadConv },
        { TIMELINE_CONTEXT_MENU_CODE, PROJECTSCENE_TIMELINE_CONTEXT_MENU_COMMAND, {} },
        { PLAY_POSITION_DECREASE_CODE, PROJECTSCENE_PLAY_POSITION_DECREASE_COMMAND, {} },
        { PLAY_POSITION_INCREASE_CODE, PROJECTSCENE_PLAY_POSITION_INCREASE_COMMAND, {} },
        { SEL_EXT_LEFT_CODE, PROJECTSCENE_SELECTION_EXTEND_LEFT_COMMAND, {} },
        { SEL_EXT_RIGHT_CODE, PROJECTSCENE_SELECTION_EXTEND_RIGHT_COMMAND, {} },
        { SEL_CNTR_LEFT_CODE, PROJECTSCENE_SELECTION_CONTRACT_LEFT_COMMAND, {} },
        { SEL_CNTR_RIGHT_CODE, PROJECTSCENE_SELECTION_CONTRACT_RIGHT_COMMAND, {} },
        { CURS_SEL_START_CODE, PROJECTSCENE_CURSOR_TO_SELECTION_START_COMMAND, {} },
        { CURS_SEL_END_CODE, PROJECTSCENE_CURSOR_TO_SELECTION_END_COMMAND, {} },
        { TOGGLE_EFFECTS_CODE, PROJECTSCENE_TOGGLE_EFFECTS_PANEL_COMMAND, {} },
        { ADD_REALTIME_EFFECTS_CODE, PROJECTSCENE_ADD_REALTIME_EFFECTS_COMMAND, {} },
        { AUDIO_SETUP_CODE, PROJECTSCENE_AUDIO_SETUP_COMMAND, {} },
        { GET_EFFECTS_CODE, PROJECTSCENE_GET_EFFECTS_COMMAND, {} },
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

void ProjectSceneActionsController::setTimelineViewController(ITimelineViewController* controller)
{
    m_timelineViewController = controller;
}

ITimelineViewController* ProjectSceneActionsController::timelineViewController() const
{
    return m_timelineViewController;
}

void ProjectSceneActionsController::setPlayPositionViewController(IPlayPositionViewController* controller)
{
    m_playPositionViewController = controller;
}

IPlayPositionViewController* ProjectSceneActionsController::playPositionViewController() const
{
    return m_playPositionViewController;
}

muse::async::Notification ProjectSceneActionsController::effectsPanelFocusRequested() const
{
    return m_effectsPanelFocusRequested;
}

muse::async::Notification ProjectSceneActionsController::audioSetupContextMenuRequested() const
{
    return m_audioSetupContextMenuRequested;
}

muse::async::Notification ProjectSceneActionsController::timelineContextMenuRequested() const
{
    return m_timelineContextMenuRequested;
}

muse::Ret ProjectSceneActionsController::toggleEffectsPanel()
{
    const muse::ui::INavigationSection* section = navigationController()->activeSection();
    if (section && section->type() == muse::ui::INavigationSection::Type::Exclusive) {
        return make_ret(Ret::Code::NotSupported);
    }

    const bool shouldShow = !configuration()->isEffectsPanelVisible();
    configuration()->setIsEffectsPanelVisible(shouldShow);
    if (shouldShow) {
        m_effectsPanelFocusRequested.notify();
    }

    return make_ok();
}

muse::Ret ProjectSceneActionsController::requestAudioSetupContextMenu()
{
    m_audioSetupContextMenuRequested.notify();
    return make_ok();
}

muse::Ret ProjectSceneActionsController::openGetEffectsDialog()
{
    interactive()->open(GET_EFFECTS_URI);
    return make_ok();
}

template<typename ViewController>
void ProjectSceneActionsController::registerViewCommand(const Command& command, ViewController* ProjectSceneActionsController::* view,
                                                        void (ViewController::* handler)())
{
    commandDispatcher()->onRequest(this, command, [this, view, handler]() {
        ViewController* controller = this->*view;
        if (!controller) {
            return make_ret(Ret::Code::NotSupported);
        }

        (controller->*handler)();
        return make_ok();
    });
}

muse::Ret ProjectSceneActionsController::centerViewOnPlayhead(const Params& params)
{
    if (!params.contains(ONLY_IF_PLAYHEAD_NOT_VISIBLE_PARAM)) {
        return make_ret(Ret::Code::BadArgs);
    }

    if (!m_timelineViewController) {
        return make_ret(Ret::Code::NotSupported);
    }

    m_timelineViewController->centerViewOnPlayhead(params.at(ONLY_IF_PLAYHEAD_NOT_VISIBLE_PARAM).toBool());
    return make_ok();
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
