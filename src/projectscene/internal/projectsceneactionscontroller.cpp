/*
* Audacity: A Digital Audio Editor
*/

#include "project/iaudacityproject.h"

#include "projectsceneactionscontroller.h"

using namespace muse;
using namespace au::projectscene;
using namespace muse::async;
using namespace muse::actions;

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
static const ActionQuery SET_TRACK_VIEW_WAVEFORM("action://projectscene/track-view-waveform");
static const ActionQuery SET_TRACK_VIEW_SPECTROGRAM("action://projectscene/track-view-spectrogram");
static const ActionQuery SET_TRACK_VIEW_MULTI("action://projectscene/track-view-multi");
static const ActionQuery TOGGLE_GLOBAL_VIEW_SPECTROGRAM("action://projectscene/global-view-spectrogram");
static const ActionCode LABEL_OPEN_EDITOR_CODE("open-label-editor");
static const ActionCode CLIP_GAIN_CODE("clip-gain");

static const muse::Uri EDIT_PITCH_AND_SPEED_URI("audacity://projectscene/editpitchandspeed");

void ProjectSceneActionsController::init()
{
    dispatcher()->reg(this, MINUTES_SECONDS_RULER, this, &ProjectSceneActionsController::toggleMinutesSecondsRuler);
    dispatcher()->reg(this, BEATS_MEASURES_RULER, this, &ProjectSceneActionsController::toggleBeatsMeasuresRuler);
    dispatcher()->reg(this, VERTICAL_RULERS_CODE, this, &ProjectSceneActionsController::toggleVerticalRulers);
    dispatcher()->reg(this, RMS_IN_WAVEFORM_CODE, this, &ProjectSceneActionsController::toggleRMSInWaveform);
    dispatcher()->reg(this, CLIPPING_IN_WAVEFORM_CODE, this, &ProjectSceneActionsController::toggleClippingInWaveform);
    dispatcher()->reg(this, TOGGLE_UPDATE_DISPLAY_WHILE_PLAYING_CODE, this,
                      &ProjectSceneActionsController::toggleUpdateDisplayWhilePlaying);
    dispatcher()->reg(this, TOGGLE_PINNED_PLAY_HEAD_CODE, this, &ProjectSceneActionsController::togglePinnedPlayHead);
    dispatcher()->reg(this, CLIP_PITCH_AND_SPEED_CODE, this, &ProjectSceneActionsController::openClipPitchAndSpeedEdit);
    dispatcher()->reg(this, TOGGLE_PLAYBACK_ON_RULER_CLICK_ENABLED_CODE, this,
                      &ProjectSceneActionsController::togglePlaybackOnRulerClickEnabled);
    dispatcher()->reg(this, TOGGLE_TRACK_HALF_WAVE, this, &ProjectSceneActionsController::toggleTrackHalfWave);
    dispatcher()->reg(this, SET_TRACK_VIEW_WAVEFORM, this, &ProjectSceneActionsController::changeTrackViewToWaveform);
    dispatcher()->reg(this, SET_TRACK_VIEW_SPECTROGRAM, this, &ProjectSceneActionsController::changeTrackViewToSpectrogram);
    dispatcher()->reg(this, SET_TRACK_VIEW_MULTI, this, &ProjectSceneActionsController::changeTrackViewToWaveformAndSpectrogram);
    dispatcher()->reg(this, TOGGLE_GLOBAL_VIEW_SPECTROGRAM, this, &ProjectSceneActionsController::toggleGlobalSpectrogramView);
    dispatcher()->reg(this, LABEL_OPEN_EDITOR_CODE, this, &ProjectSceneActionsController::openLabelEditor);
    dispatcher()->reg(this, CLIP_GAIN_CODE, this, &ProjectSceneActionsController::toggleAutomation);
}

void ProjectSceneActionsController::notifyActionCheckedChanged(const ActionCode& actionCode)
{
    m_actionCheckedChanged.send(actionCode);
}

void ProjectSceneActionsController::toggleMinutesSecondsRuler()
{
    if (configuration()->timelineRulerMode() == TimelineRulerMode::MINUTES_AND_SECONDS) {
        return;
    }

    configuration()->setTimelineRulerMode(TimelineRulerMode::MINUTES_AND_SECONDS);
    notifyActionCheckedChanged(MINUTES_SECONDS_RULER);
    notifyActionCheckedChanged(BEATS_MEASURES_RULER);
}

void ProjectSceneActionsController::toggleBeatsMeasuresRuler()
{
    if (configuration()->timelineRulerMode() == TimelineRulerMode::BEATS_AND_MEASURES) {
        return;
    }

    configuration()->setTimelineRulerMode(TimelineRulerMode::BEATS_AND_MEASURES);
    notifyActionCheckedChanged(MINUTES_SECONDS_RULER);
    notifyActionCheckedChanged(BEATS_MEASURES_RULER);
}

void ProjectSceneActionsController::toggleVerticalRulers()
{
    bool verticalRulersVisible = configuration()->isVerticalRulersVisible();
    configuration()->setVerticalRulersVisible(!verticalRulersVisible);
    notifyActionCheckedChanged(VERTICAL_RULERS_CODE);
}

void ProjectSceneActionsController::toggleRMSInWaveform()
{
    bool rmsVisible = configuration()->isRMSInWaveformVisible();
    configuration()->setRMSInWaveformVisible(!rmsVisible);
    notifyActionCheckedChanged(RMS_IN_WAVEFORM_CODE);
}

void ProjectSceneActionsController::toggleClippingInWaveform()
{
    bool clippingVisible = configuration()->isClippingInWaveformVisible();
    configuration()->setClippingInWaveformVisible(!clippingVisible);
    notifyActionCheckedChanged(CLIPPING_IN_WAVEFORM_CODE);
}

void ProjectSceneActionsController::toggleUpdateDisplayWhilePlaying()
{
    bool enabled = configuration()->updateDisplayWhilePlayingEnabled();
    configuration()->setUpdateDisplayWhilePlayingEnabled(!enabled);
    notifyActionCheckedChanged(TOGGLE_UPDATE_DISPLAY_WHILE_PLAYING_CODE);
}

void ProjectSceneActionsController::togglePinnedPlayHead()
{
    bool enabled = configuration()->pinnedPlayHeadEnabled();
    configuration()->setPinnedPlayHeadEnabled(!enabled);
    notifyActionCheckedChanged(TOGGLE_PINNED_PLAY_HEAD_CODE);
}

void ProjectSceneActionsController::openClipPitchAndSpeedEdit(const ActionData& args)
{
    if (interactive()->isOpened(EDIT_PITCH_AND_SPEED_URI).val) {
        return;
    }

    IF_ASSERT_FAILED(args.count() == 1) {
        return;
    }

    trackedit::ClipKey clipKey = args.arg<trackedit::ClipKey>(0);
    if (!clipKey.isValid()) {
        return;
    }

    muse::UriQuery query(EDIT_PITCH_AND_SPEED_URI);
    query.addParam("trackId", muse::Val(std::to_string(clipKey.trackId)));
    query.addParam("clipId", muse::Val(std::to_string(clipKey.itemId)));
    query.addParam("focusItemName", muse::Val("pitch"));

    interactive()->open(query);
}

void ProjectSceneActionsController::openLabelEditor()
{
    interactive()->open("audacity://projectscene/openlabeleditor");
}

void ProjectSceneActionsController::togglePlaybackOnRulerClickEnabled()
{
    bool isEnabled = configuration()->playbackOnRulerClickEnabled();
    configuration()->setPlaybackOnRulerClickEnabled(!isEnabled);
    notifyActionCheckedChanged(TOGGLE_PLAYBACK_ON_RULER_CLICK_ENABLED_CODE);
}

void ProjectSceneActionsController::toggleAutomation()
{
    project::IAudacityProjectPtr prj = globalContext()->currentProject();
    const auto viewState = prj->viewState();

    if (viewState == nullptr) {
        return;
    }

    const bool automationState = viewState->clipGainAutomationEnabled().val;
    const bool enablingAutomation = !automationState;
    if (enablingAutomation && viewState->globalSpectrogramToggleIsOn()) {
        viewState->toggleGlobalSpectrogramView();
    }

    viewState->setClipGainAutomationEnabled(enablingAutomation);
}

void ProjectSceneActionsController::toggleGlobalSpectrogramView()
{
    const auto project = globalContext()->currentProject();
    IF_ASSERT_FAILED(project) {
        return;
    }

    const auto viewState = project->viewState();
    IF_ASSERT_FAILED(viewState) {
        return;
    }

    const bool enablingGlobalSpectrogram = !viewState->globalSpectrogramToggleIsOn();
    if (enablingGlobalSpectrogram && viewState->clipGainAutomationEnabled().val) {
        viewState->setClipGainAutomationEnabled(false);
    }

    viewState->toggleGlobalSpectrogramView();
}

void ProjectSceneActionsController::toggleTrackHalfWave(const muse::actions::ActionQuery& q)
{
    IF_ASSERT_FAILED(q.params().size() >= 1) {
        return;
    }
    const int trackId = q.param("trackId").toInt();

    project::IAudacityProjectPtr prj = globalContext()->currentProject();
    const auto viewState = prj->viewState();

    if (viewState == nullptr) {
        return;
    }
    viewState->toggleHalfWave(trackId);
    notifyActionCheckedChanged(TOGGLE_TRACK_HALF_WAVE.toString());
}

void ProjectSceneActionsController::changeTrackViewToWaveform(const muse::actions::ActionQuery& q)
{
    changeTrackView(q, TrackViewType::Waveform);
}

void ProjectSceneActionsController::changeTrackViewToSpectrogram(const muse::actions::ActionQuery& q)
{
    changeTrackView(q, TrackViewType::Spectrogram);
}

void ProjectSceneActionsController::changeTrackViewToWaveformAndSpectrogram(const muse::actions::ActionQuery& q)
{
    changeTrackView(q, TrackViewType::WaveformAndSpectrogram);
}

void ProjectSceneActionsController::changeTrackView(const muse::actions::ActionQuery& q, TrackViewType trackView)
{
    IF_ASSERT_FAILED(q.params().size() >= 1) {
        return;
    }
    const auto trackId = q.param("trackId").toInt();
    const auto prj = globalContext()->currentProject();
    IF_ASSERT_FAILED(prj) {
        return;
    }
    const auto viewState = prj->viewState();
    IF_ASSERT_FAILED(viewState) {
        return;
    }
    viewState->setTrackViewType(trackId, trackView);
    switch (trackView) {
    case TrackViewType::Waveform:
        notifyActionCheckedChanged(SET_TRACK_VIEW_WAVEFORM.toString());
        break;
    case TrackViewType::Spectrogram:
        notifyActionCheckedChanged(SET_TRACK_VIEW_SPECTROGRAM.toString());
        break;
    case TrackViewType::WaveformAndSpectrogram:
        notifyActionCheckedChanged(SET_TRACK_VIEW_MULTI.toString());
        break;
    default:
        assert(false);
    }
}

bool ProjectSceneActionsController::actionChecked(const ActionCode& actionCode) const
{
    QMap<std::string, bool> isChecked {
        { VERTICAL_RULERS_CODE, configuration()->isVerticalRulersVisible() },
        { RMS_IN_WAVEFORM_CODE, configuration()->isRMSInWaveformVisible() },
        { CLIPPING_IN_WAVEFORM_CODE, configuration()->isClippingInWaveformVisible() },
        { MINUTES_SECONDS_RULER, configuration()->timelineRulerMode() == TimelineRulerMode::MINUTES_AND_SECONDS },
        { BEATS_MEASURES_RULER, configuration()->timelineRulerMode() == TimelineRulerMode::BEATS_AND_MEASURES },
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
