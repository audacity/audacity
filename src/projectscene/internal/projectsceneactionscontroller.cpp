/*
* Audacity: A Digital Audio Editor
*/

#include "project/iaudacityproject.h"

#include "projectsceneactionscontroller.h"
#include "../projectscenecommands.h"

using namespace muse;
using namespace au::projectscene;
using namespace muse::rcommand;

static const muse::Uri EDIT_PITCH_AND_SPEED_URI("audacity://projectscene/editpitchandspeed");

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
    registerViewCommand(PROJECTSCENE_TIMELINE_PAGE_LEFT_COMMAND, &ProjectSceneActionsController::m_timelineViewController,
                        &ITimelineViewController::pageLeft);
    registerViewCommand(PROJECTSCENE_TIMELINE_PAGE_RIGHT_COMMAND, &ProjectSceneActionsController::m_timelineViewController,
                        &ITimelineViewController::pageRight);
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
    cd->onRequest(this, PROJECTSCENE_AUDIO_SETUP_COMMAND, [this]() { return requestAudioSetupContextMenu(); });
    cd->onRequest(this, PROJECTSCENE_TOGGLE_SPLIT_TOOL_COMMAND, [this]() {
        m_splitToolToggleRequested.notify();
        return make_ok();
    });
    cd->onRequest(this, PROJECTSCENE_REALTIME_EFFECT_MOVE_UP_COMMAND, [this]() {
        m_realtimeEffectMoveUpRequested.notify();
        return make_ok();
    });
    cd->onRequest(this, PROJECTSCENE_REALTIME_EFFECT_MOVE_DOWN_COMMAND, [this]() {
        m_realtimeEffectMoveDownRequested.notify();
        return make_ok();
    });

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

muse::async::Notification ProjectSceneActionsController::splitToolToggleRequested() const
{
    return m_splitToolToggleRequested;
}

muse::async::Notification ProjectSceneActionsController::realtimeEffectMoveUpRequested() const
{
    return m_realtimeEffectMoveUpRequested;
}

muse::async::Notification ProjectSceneActionsController::realtimeEffectMoveDownRequested() const
{
    return m_realtimeEffectMoveDownRequested;
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
    if (!params.contains(PROJECTSCENE_CENTER_VIEW_ON_PLAYHEAD_ONLY_IF_NOT_VISIBLE_PARAM)) {
        return make_ret(Ret::Code::BadArgs);
    }

    if (!m_timelineViewController) {
        return make_ret(Ret::Code::NotSupported);
    }

    m_timelineViewController->centerViewOnPlayhead(params.at(PROJECTSCENE_CENTER_VIEW_ON_PLAYHEAD_ONLY_IF_NOT_VISIBLE_PARAM).toBool());
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
    return make_ok();
}

muse::Ret ProjectSceneActionsController::toggleRMSInWaveform()
{
    bool rmsVisible = configuration()->isRMSInWaveformVisible();
    configuration()->setRMSInWaveformVisible(!rmsVisible);
    return make_ok();
}

muse::Ret ProjectSceneActionsController::toggleClippingInWaveform()
{
    bool clippingVisible = configuration()->isClippingInWaveformVisible();
    configuration()->setClippingInWaveformVisible(!clippingVisible);
    return make_ok();
}

muse::Ret ProjectSceneActionsController::toggleUpdateDisplayWhilePlaying()
{
    bool enabled = configuration()->updateDisplayWhilePlayingEnabled();
    configuration()->setUpdateDisplayWhilePlayingEnabled(!enabled);
    return make_ok();
}

muse::Ret ProjectSceneActionsController::togglePinnedPlayHead()
{
    bool enabled = configuration()->pinnedPlayHeadEnabled();
    configuration()->setPinnedPlayHeadEnabled(!enabled);
    return make_ok();
}

muse::Ret ProjectSceneActionsController::openClipPitchAndSpeedEdit(const Params& params)
{
    if (interactive()->isOpened(EDIT_PITCH_AND_SPEED_URI).val) {
        return make_ret(Ret::Code::Busy);
    }

    IF_ASSERT_FAILED(params.contains(PROJECTSCENE_CLIP_PITCH_AND_SPEED_TRACK_ID_PARAM)
                     && params.contains(PROJECTSCENE_CLIP_PITCH_AND_SPEED_CLIP_ID_PARAM)) {
        return make_ret(Ret::Code::BadArgs);
    }

    const trackedit::ClipKey clipKey(params.at(PROJECTSCENE_CLIP_PITCH_AND_SPEED_TRACK_ID_PARAM).toInt64(),
                                     params.at(PROJECTSCENE_CLIP_PITCH_AND_SPEED_CLIP_ID_PARAM).toInt64());
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
    IF_ASSERT_FAILED(params.contains(PROJECTSCENE_TOGGLE_TRACK_HALF_WAVE_TRACK_ID_PARAM)) {
        return make_ret(Ret::Code::BadArgs);
    }
    const int trackId = params.at(PROJECTSCENE_TOGGLE_TRACK_HALF_WAVE_TRACK_ID_PARAM).toInt();

    project::IAudacityProjectPtr prj = globalContext()->currentProject();
    if (!prj) {
        return make_ret(Ret::Code::NotSupported);
    }

    const auto viewState = prj->viewState();
    if (viewState == nullptr) {
        return make_ret(Ret::Code::NotSupported);
    }

    viewState->toggleHalfWave(trackId);
    return make_ok();
}
